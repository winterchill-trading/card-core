Response codes and whether a merchant may retry:

  $ card codes
  00  approved
  05  Generic              retry allowed
  14  Never_approve        do not retry
  46  Never_approve        do not retry
  51  Cannot_approve_now   retry allowed
  54  Data_quality         retry allowed
  55  Data_quality         retry allowed
  57  Never_approve        do not retry
  61  Cannot_approve_now   retry allowed
  62  Never_approve        do not retry
  65  Cannot_approve_now   retry allowed
  83  Never_approve        do not retry
  91  Cannot_approve_now   retry allowed
  1A  Data_quality         retry allowed

Hold validity by merchant category:

  $ card hold-expiry -mcc 5411
  7d
  $ card hold-expiry -mcc 7011
  30d
  $ card hold-expiry -mcc 5411 -estimated
  30d

A fixed scenario from authorization to refund:

  $ card demo
    book evt-deposit              DEPOSIT          200.00 EUR
  a1 COFFEE BAR 4.20 EUR -> 00 (Approve(amount"4.20 EUR")(partial false)(sca(Exempt Contactless)))
    book evt-a1/hold              CARD_AUTH        4.20 EUR
    available 195.80 EUR, held 4.20 EUR
  a2 RESTAURANT 40.00 EUR -> 00 (Approve(amount"40.00 EUR")(partial false)(sca(Exempt Contactless)))
    book evt-a2/hold              CARD_AUTH        40.00 EUR
    available 155.80 EUR, held 44.20 EUR
  a3 ELECTRONICS 600.00 EUR -> 61 (Decline(Limit Per_transaction))
    available 155.80 EUR, held 44.20 EUR
  c1 Debit 4.20 EUR
    book evt-c1/settle            CARD_SETTLEMENT  4.20 EUR
    available 155.80 EUR, held 40.00 EUR
  c2 Debit 46.00 EUR
    book evt-c2/settle            CARD_SETTLEMENT  46.00 EUR
    available 149.80 EUR, held 0.00 EUR
  c3 Credit 4.20 EUR
    book evt-c3/refund            REFUND           4.20 EUR
    available 154.00 EUR, held 0.00 EUR
  Card **** 4242 10/29, 2026-10-13 to 2026-11-12
  2026-10-14  purchase     COFFEE BAR                      -4.20 EUR
  2026-10-14  purchase     RESTAURANT                     -46.00 EUR
  2026-10-21  refund       COFFEE BAR                       4.20 EUR
  spent                                                     50.20 EUR
  refunded                                                   4.20 EUR
  pending                                                    0.00 EUR
