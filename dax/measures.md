Approval Rate = DIVIDE(
    COUNTROWS(FILTER(Applications, Applications[Approved] = TRUE())),
    COUNTROWS(FILTER(Applications, Applications[Status] <> "Pending")),
    0
)

Average Credit History (Years) = ROUND( 
    AVERAGEX( VALUES ( CreditHistory[CustomerID] ), 
    DATEDIFF( CALCULATE ( MIN ( CreditHistory[Date] ) ), 
    MAX ( DimDate[Date] ), YEAR ) ), 0 )

Average Customer Value = DIVIDE( sum(DimCustomers[Income]),COUNTROWS(DimCustomers))

Customer Total Debt = CALCULATE ( 
    SUM ( DimCustomers[Credit.TotalDebt] ), T
          REATAS ( VALUES ( DimCustomers[CustomerID] ), DimCustomers[CustomerID] ) )

Customers Defaulted Number = CALCULATE(  
      CALCULATE(COUNTROWS(DimCustomers), DimCustomers[HasDefaultedBefore] = 1), 
      KEEPFILTERS( VALUES( Applications[LoanPurpose] ) ), 
      CROSSFILTER( DimCustomers[CustomerID], Applications[CustomerID], BOTH ) )

Customers Defaulted Rate (%) = CALCULATE( 
    DIVIDE( CALCULATE(DISTINCTCOUNT(DimCustomers[CustomerID]), 
            DimCustomers[HasDefaultedBefore] = 1), COUNTROWS(DimCustomers),0), 
    KEEPFILTERS( VALUES( Applications[LoanPurpose] ) ), 
    CROSSFILTER( DimCustomers[CustomerID], Applications[CustomerID], BOTH ) )

Defaulted Loans Rate = DIVIDE(
    CALCULATE(COUNTROWS(Applications),Applications[Status]="Default"),
    COUNTROWS(Applications),0)

DefaultRate = 
DIVIDE (
    CALCULATE ( COUNTROWS (Applications), Applications[Status] = "Default" ),
    COUNTROWS ( Applications ),
    0)

First Year Loan = CALCULATE(
      sum(Applications[LoanAmount]),
      (Year(Applications[ApplicationDate])=2023 || (Year(Applications[ApplicationDate])=2024 &&
       Month(Applications[ApplicationDate])<=5)))

Second Year Loan = CALCULATE(sum(Applications[LoanAmount]),
    (Year(Applications[ApplicationDate])=2025 || (Year(Applications[ApplicationDate])=2024 && 
     Month(Applications[ApplicationDate])>5)))

    
Fraudulent Transactions Rate = DIVIDE(
    COUNTROWS(FILTER(Transactions, Transactions[IsFraud] = 1)),
    COUNTROWS(Transactions),
    0)

FraudWeighted Concentration = 
DIVIDE (
    SUMX ( Applications, Applications[LoanAmount] * Applications[IsFraudApp] ),
    SUM ( Applications[LoanAmount] ),
    0)

New Customers (Last 30 Days) = 
VAR MaxAppDate = CALCULATE ( MAX ( Applications[ApplicationDate] ), ALL ( Applications ) ) 
RETURN 
CALCULATE ( 
    DISTINCTCOUNT ( Applications[CustomerID] ), 
    FILTER ( VALUES ( Applications[CustomerID] ), CALCULATE ( MIN ( Applications[ApplicationDate] ) ) >= MaxAppDate - 30 
    && CALCULATE ( MIN ( Applications[ApplicationDate] ) ) <= MaxAppDate ) 
    )

Subprime Customers = DIVIDE(
    CALCULATE(COUNTROWS(DimCustomers), DimCustomers[CreditScoreTier] = "Poor"),
    COUNTROWS(DimCustomers),0)

Top 3 Regions Fraud = 
CONCATENATEX(
    TOPN(
        3,
        SUMMARIZE(
            Applications,
            Applications[Region],
            "FraudCount", [Fraud Loan Number]
        ),
        [FraudCount], DESC
    ),
    Applications[Region] & ": " & [FraudCount],
    UNICHAR(10)
)

YoY Loan Amount = DIVIDE( ([Second Year Loan]-[First Year Loan]), [First Year Loan]) 
