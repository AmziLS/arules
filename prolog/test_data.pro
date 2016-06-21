:- include('opdefs.pro').
:- discontiguous(r/3).
:- discontiguous(q/2).
:- discontiguous(q/3).
:- discontiguous(a/2).
:- discontiguous(d/3).
:- discontiguous(case/2).
:- op(600, fx, test).


%-----------------------------------------------------
case(1, [ rulesets = [one] ]).


r(one, a1, "call.rate = 0.05").
r(one, a2, "rate = 'free'").
r(one, a3, "call[3].rate = 0.02").
r(one, a4, "weekend WHEN call.day = 'Saturday' OR call.day = 'Sunday'").
r(one, a6, "age = 1 year 12 days").
r(one, a7, "young = 1 WHEN age < 24 months").
r(one, a8, "young = 2").
r(one, a9, "something").
r(one, a10, "else = 'false' when something = 'true'").
   
q(one, "find call.rate", 0.05).
q(one, "find rate", free).
q(one, "find call[3].rate", 0.02).
q(one, "find young", 1).
q(one, "find else", false).

%---------------------------------------------------------
case(2, [ rulesets = [two], ask_answers = [10,10] ]).

r(two, a1, "call.price = call.rate * call.duration").
r(two, a2, "call.rate = 0.05 when weekend").
r(two, a3, "call.rate = 0.09 when not weekend").
r(two, a4, "weekend when call.date.day = 'Sunday' or call.date.day = 'Saturday'").
r(two, a6, "call.duration = ask('How long was the call? (say 10)')").

q(two, "find call.price when call.rate = 5.0e-02 and call.duration = 10", 0.5).
q(two, "find call.price when call.date.day = 'saturday'", 0.5).
q(two, "find call.price when call.date.day = 'tuesday'", 0.9).

%-------------------------------------------------
case(3, [ rulesets = [three], ask_answers = [10,10] ]).

r(three, a1, "call.price = call.rate * call.duration").
r(three, a2, "call.rate = 0.05 when weekend").
r(three, a3, "call.rate = 0.09 when not weekend").
r(three, a4, "weekend when day_type[call.date.day, 'weekend']").
r(three, a5, "day_type[?day, 'weekend'] when ?day = 'saturday' or ?day = 'sunday'").
r(three, a6, "day_type[?day, 'weekday'] when ?day = member(['monday', 'tuesday', 'wednesday', 'thursday', 'friday'])").
r(three, a7, "call.duration = ask('How long was the call?')").

q(three, "find call.price when call.date.day = 'saturday'", 0.5).
q(three, "find call.price when call.date.day = 'tuesday'", 0.9).

%----------------------------------------------------
case(4, [ rulesets = [four, fourDays] ]).

r(four, a1, "call.rate = 0.05 when fourDays:weekend").
r(four, a2, "call.rate = 0.09 when fourDays!weekday").

r(fourDays, a1, "weekend when day = 'saturday' or day = 'sunday'").
r(fourDays, a2, "weekday when not weekend").

q(four, "find call.rate when fourDays:day = 'saturday'", 0.05).
q(four, "find call.rate when fourDays!day = 'tuesday'", 0.09).

%-------------------------------------------------------------
case(5, [ rulesets = [five, fiveDays] ]).

r(five, a0, "Inherit from fiveDays").
r(five, a1, "call.rate = 0.05 when daytype[call.day,'weekend']").
r(five, a2, "call.rate = 0.09 when daytype[call.day,'weekday']").

r(fiveDays, a1, "daytype[?day,'weekend'] when ?day = 'saturday' or ?day = 'sunday'").
r(fiveDays, a2, "daytype[?day,'weekday'] when ?day = member(['monday', 'tuesday', 'wednesday', 'thursday', 'friday'])").

q(five, "find call.rate when call.day = 'saturday'", 0.05).
q(five, "find call.rate when call.day = 'tuesday'", 0.09).

%---------------------------------------------------------------------
case(6, [ rulesets = [six, sixRate] ]).

r(six, a1, "call.price = sixRate:call.rate * call.duration").

r(sixRate, a1, "call.rate = 0.05 when call.day = 'saturday' or call.day = 'sunday'").
r(sixRate, a2, "call.rate = 0.09").

q(six, "find call.price when sixRate!call.day = 'monday' and call.duration = 11", 0.99).

%--------------------------------------------------------------
case(7, [ rulesets = [seven] ]).

r(seven, a1, "Part['gizmo'].UnitPrice = 10 when Part['gizmo'].Quantity < Part['gizmo'].DiscountQuantity").
r(seven, a2, "Part['gizmo'].UnitPrice = 20 when Part['gizmo'].Quantity >= Part['gizmo'].DiscountQuantity").
r(seven, a3, "Part['widget'].UnitPrice = 10 when Part['widget'].Quantity < Part['widget'].DiscountQuantity").
r(seven, a4, "Part['widget'].UnitPrice = 20 when Part['widget'].Quantity >= Part['widget'].DiscountQuantity").
r(seven, a5, "Part['gizmo'].DiscountQuantity = 5").
r(seven, a6, "Part['widget'].DiscountQuantity = 10").
r(seven, a7, "Order.Price = Part['gizmo'].Quantity * Part['gizmo'].UnitPrice + Part['widget'].Quantity * Part['widget'].UnitPrice").

q(seven, "Find Order.Price when Part['gizmo'].quantity = 7 and Part['widget'].quantity = 7", 210).

%-----------------------------------------------------------------
case(8, [ rulesets = [eight] ]).

r(eight, a1, "Part[?p].UnitPrice = 10 when Part[?p].Quantity < Part[?p].DiscountQuantity").
r(eight, a2, "Part[?p].UnitPrice = 20 when Part[?p].Quantity >= Part[?p].DiscountQuantity").
r(eight, a3, "Part['gizmo'].DiscountQuantity = 5").
r(eight, a4, "Part['widget'].DiscountQuantity = 10").
r(eight, a5, "Order.Price = Part['gizmo'].Quantity * Part['gizmo'].UnitPrice + Part['widget'].Quantity * Part['widget'].UnitPrice").

q(eight, "Find Order.Price when Part['gizmo'].quantity = 7 and Part['widget'].quantity = 7", 210).

%-----------------------------------------------------------------

case(9, [ rulesets = [nine] ]).

r(nine, a1, "get_av{?a, ?v, ?l} when mimber{ [?a,?v], ?l }").
r(nine, a2, "avs = [['color','blue'], ['wheels',4], ['doors',2]]").
r(nine, a3, "value = ?v when get_av{attr, ?v, avs}").

q(nine, "find value when attr = 'wheels'", 4).

%-----------------------------------------------------------------
case(10, [ rulesets = [ten, gizmo, widget] ]).

r(ten, a1, "price = quantity * unitprice").
r(ten, a2, "unitprice = gizmo!unitprice when part = 'gizmo'").
r(ten, a3, "unitprice = widget!unitprice when part = 'widget'").
r(gizmo, a4, "unitprice = 10 when ten!quantity > 10").
r(gizmo, a5, "unitprice = 8 when ten!quantity <= 10").
r(widget, a6, "unitprice = 5 when ten!quantity > 10").
r(widget, a7, "unitprice = 4 when ten!quantity <= 10").

q(ten, "find price when quantity = 12 and part = 'widget'", 60).
q(ten, "find price when quantity = 12 and part = 'gizmo'", 120).

%--------------------------------------------------------------------------------------
case(11, [ rulesets = [PriceRules11] ]).

r(PriceRules11, A34, "price = duration *  rate when duration <= OverTime").
r(PriceRules11, A35, "price = rate * duration + Surcharge * (duration - Overtime) when duration > overtime").
r(PriceRules11, A36, "rate = 0.05  WHEN weekend ").
r(PriceRules11, A37, "rate = 0.10 WHEN weekday AND daytime").
r(PriceRules11, A38, "rate = 0.07 WHEN weekday AND nighttime").
r(PriceRules11, A39, "weekend WHEN day = 'Sunday' OR day = 'Saturday'").
r(PriceRules11, A40, "weekend WHEN day = 'Friday' AND time > FridayWeekendTime").
r(PriceRules11, A41, "weekday WHEN NOT weekend").
r(PriceRules11, A42, "nighttime WHEN time >= 2000 OR time =< 500").
r(PriceRules11, A43, "daytime WHEN NOT nighttime").
r(PriceRules11, A44, "Surcharge = 0.7 ").
r(PriceRules11, A45, "OverTime = 9 ").
r(PriceRules11, A46, "FridayWeekendTime = 2350").

q(PriceRules11, "FIND price WHEN day = 'Saturday' AND time = 2200 and duration = 11", 1.95).
q(PriceRules11, "FIND price WHEN day = 'wednesday' AND time = 2200 and duration = 11", 2.17).


%-----------------------------------------------------------------
case(12, [ rulesets = [r12] ]).

r(r12, a1, "report = section1 & section2").
r(r12, a2, "section1 = 'A good section ' & 1 when goodone").
r(r12, a3, "section1 = 'A bad section ' & 1 when not goodone").
r(r12, a4, "section2 = ' A complex section ' & 2 & ' because ' & reason when complex").
r(r12, a5, "section2 = ' A simple section ' & 2").
r(r12, a6, "reason = 'Just because' when complex").
r(r12, a7, "goodone = 'false'").
r(r12, a8, "complex = 'false'").

q(r12, "find report when goodone and complex",
          "A good section 1 A complex section 2 because Just because").
q(r12, "find report when goodone",
          "A good section 1 A simple section 2").
q(r12, "find report when complex",
          "A bad section 1 A complex section 2 because Just because").

%-----------------------------------------------------------------
case(13, [ rulesets = [r13] ]).

r(r13, a1, "a when b").
r(r13, a2, "b = 'falst'").

q(r13, "find a", false).

%-----------------------------------------------------------------
case(14, [ rulesets = [r14, part14, thing14, gizmo14, widget14] ]).

r(r14, a1, "part['gizmo'].color = gizmo14!color").
r(r14, a2, "part['widget'].color = widget14!color").
r(r14, a3, "color = ?x!color when ?x = part").
r(r14, a4, "blop = ?x when ?x = input").

r(thing14, a5, "color = 'green'").

r(part14, a6, "sky = 'blue'").

r(gizmo14, a7, "inherit from part14 thing14").
r(gizmo14, a8, "unitprice = 7").

r(widget14, a9, "inherit from part14").
r(widget14, a10, "color = 'red'").
r(widget14, a11, "unitprice = 10").

q(r14, "find part['widget'].color", red).
q(r14, "find part['gizmo'].color", green).
q(r14, "find blop when input = 'green'", green).
q(r14, "find color when part = 'widget14'", red).
q(r14, "find color when part = 'gizmo14'", green).

%-----------------------------------------------------------------
case(15, [ rulesets = [r15] ]).

r(r15, a1, "cutoff_date = date(2004, 7, 15)").
r(r15, a2, "rebate = 'full' when purchase_date < cutoff_date").
r(r15, a3, "rebate = 'partial' when purchase_date < cutoff_date + 2 months").
r(r15, a4, "rebate = 'none'").
r(r15, a5, "license = 'drivers' when age > 18 years").
r(r15, a6, "license = 'student' when age > 16 years 6 months").
r(r15, a7, "license = 'not yet'").
r(r15, a8, "renew_date = last_license_date + 2 years 1 months 1 days").
r(r15, a9, "age = today - birthday").
r(r15, a10, "newage = `18 years 6 months 3 days`").
r(r15, a11, "today = date(2005, 5, 12)").
r(r15, a12, "cutoff_date_string = FORMAT_DATE( cutoff_date, 'mm/dd/yyyy' )").
r(r15, a13, "age_string = AGE_TO_LISTSTRING( age )").
r(r15, a14, "day_of_cutoff = WEEKDAY( cutoff_date )").

q(r15, "find cutoff_date", "2004-7-15").
q(r15, "find age when birthday = date(1946,2,24)", "59 years 2 months 18 days").
q(r15, "find rebate when purchase_date = date(2004, 6, 15)", full).
q(r15, "find rebate when purchase_date = date(2004, 8, 15)", partial).
q(r15, "find rebate when purchase_date = date(2004, 10, 15)", none).
q(r15, "find license when birthday = date(1987, 5, 11)", drivers).
q(r15, "find license when birthday = date(1987, 5, 13)", student).
q(r15, "find license when birthday = date(1988, 11, 11)", student).
q(r15, "find license when birthday = date(1988, 11, 13)", 'not yet').
q(r15, "find renew_date when last_license_date = date(2005,2,24)", "2007-3-25").
q(r15, "find cutoff_date_string", "07/15/2004").
q(r15, "find age_string when birthday = date(1946,2,24)", "[59 years, 2 months, 18 days]" ).
q(r15, "find day_of_cutoff", "Thursday").

%-----------------------------------------------------------------
case(16, [ rulesets = [r16] ]).

r(r16, a1, "call.duration = call.end - call.start").
r(r16, a2, "call.type = 'short' when call.duration < 10 minutes").
r(r16, a3, "call.type = 'long' when call.duration >= 10 minutes").
r(r16, a4, "call.cost = 0 when WEEKDAY( call.start ) = 'Saturday'").
r(r16, a5, "call.cost = 7 when WEEKDAY( call.start ) = 'Wednesday'").

q(r16, "find call.type when call.start = datetime(2005,5,28,13,37,10) and call.end = datetime(2005,5,28,13,52,8)", long).
q(r16, "find call.type when call.start = datetime(2005,5,25,13,37,10) and call.end = datetime(2005,5,25,13,42,8)", short).
q(r16, "find call.cost when call.start = datetime(2005,5,28,13,37,10) and call.end = datetime(2005,5,28,13,52,8)", 0).
q(r16, "find call.cost when call.start = datetime(2005,5,25,13,37,10) and call.end = datetime(2005,5,25,13,42,8)", 7).

%-----------------------------------------------------------------
case(17, [ rulesets = [r17], ask_answers = [hello] ]).

r(r17, a1, "b = c  ~B6~   ").
r(r17, a2, "x = ask('What is X?')").
r(r17, a3, "bb = 3 when x = 'hello'").

q(r17, "find b when c = datetime(2005, 6, 1, 8, 15, 0)", "2005-6-1 8:15:0").
q(r17, "find bb", 3).

%-----------------------------------------------------------------
case(18, [ rulesets = [r18] ]).

r(r18, a1, "table.name = 'Table 1' when today - hiredate > 25 years").
r(r18, a2, "table.name = 'Table 2'").
r(r18, a3, "term = today - hiredate").
r(r18, a4, "a when (b - c) > 10").
r(r18, a5, "a when 10 < (b - c)").
r(r18, a6, "a = 'fals'e").
r(r18, a7, "ago = today - (25 years - 1 day)").
r(r18, a8, "today = date(2005,6,6)").


q(r18, "find table.name when hiredate = date(1970, 1, 1)", 'Table 1').
q(r18, "find table.name when hiredate = date(1990, 1, 1)", 'Table 2').
q(r18, "find a when b = 88 and c = 66", true).
q(r18, "find a when b = 88 and c = 85", false).
q(r18, "find ago", "1980-6-7").

%-----------------------------------------------------------------
case(19, [ rulesets = [r19, r19a, r19b] ]).

r(r19, a1, "a = 1").
r(r19a, a1, "a = 2").
r(r19b, a1, "a = 3").

q(r19, "find a", 1).
q(r19, "find in r19a a", 2).
q(r19, "find in r19b a", 3).
q(r19, "find a in r19a", 2).
q(r19, "find a in r19b", 3).
q(r19, "find a when x = 3", 1).
q(r19, "find in r19a a when x = 3", 2).
q(r19, "find in r19b a when x = 3", 3).
q(r19, "find a in r19a when x = 3", 2).
q(r19, "find a in r19b when x = 3", 3).

%-----------------------------------------------------------------
case(20, [ rulesets = [r20] ]).

r(r20, a1, "size = 0.5 and color = 'pink'  when crack >= 14 and crack <= 18").
r(r20, a2, "size = 1.0 and color = 'red'   when crack >= 17 and crack <= 22").
r(r20, a3, "size = 1.5 and color = 'black' when crack >= 23 and crack <= 28").
r(r20, a4, "size = 2.0 and color = 'blue'  when crack >= 26 and crack <= 31").
r(r20, a5, "price = 10 when color = 'pink'").
r(r20, a6, "price = 15 when color = 'red'").

q(r20, "find size and color when crack = 19", [1.0, red]).
q(r20, "find price when crack = 19", 15).

%-----------------------------------------------------------------
case(21, [ rulesets = [r21] ]).

r(r21, a1, "Price = BasePrice - Discount * BasePrice").
r(r21, a2, "CouponDiscount = 0.15 WHEN expiration >= today").
r(r21, a3, "CouponDiscount = 0").
r(r21, a4, "AgeDiscount = 0.30 WHEN birthday").
r(r21, a5, "AgeDiscount = 0.20 WHEN age >= 59 years 6 months").
r(r21, a6, "AgeDiscount = 0").
r(r21, a7, "Age = today - Birthdate").
r(r21, a8, "BasePrice = 10 * Quantity WHEN weekend").
r(r21, a9, "BasePrice = 8 * Quantity").
r(r21, a10, "weekend WHEN weekday( today ) = 'Saturday' OR weekday( today ) = 'Sunday'").
r(r21, a11, "weekend = false").
r(r21, a12, "Discount = MAXIMUM(CouponDiscount, AgeDiscount)").
r(r21, a14, "birthday WHEN MONTH(birthdate) = MONTH(today) AND DAY(birthdate) = DAY(today) ").
r(r21, a15, "today = date(2005,8,13)").

q(r21, "FIND price WHEN birthdate = date(1945,2,24) AND expiration = date(2005,8,8) AND quantity = 10", 80).
q(r21, "FIND baseprice WHEN birthdate = date(1945,2,24) AND expiration = date(2005,8,8) AND quantity = 10", 100).
q(r21, "FIND discount WHEN birthdate = date(1945,2,24) AND expiration = date(2005,8,8) AND quantity = 10", 0.2).
q(r21, "FIND discount WHEN birthdate = date(1946,2,24) AND expiration = date(2005,8,8) AND quantity = 10", 0).
q(r21, "FIND discount WHEN birthdate = date(1959,2,24) AND expiration = date(2005,8,8) AND quantity = 10", 0).
q(r21, "FIND discount WHEN birthdate = date(1959,2,24) AND expiration = date(2005,8,22) AND quantity = 10", 0.15).
q(r21, "FIND discount WHEN birthdate = date(1945,8,13) AND expiration = date(2005,8,22) AND quantity = 10", 0.3).
q(r21, "FIND birthday WHEN birthdate = date(1946,8,13)", true).

%-----------------------------------------------------------------
case(22, [ rulesets = [vaccines, twinrix, ipv] ]).

r(Vaccines, a1, "Vaccine[?v].Status = 'Behind' WHEN today > Vaccine[?v].RecAge2").
r(Vaccines, a2, "Vaccine[?v].Status = 'Eligible' WHEN today >= Vaccine[?v].MinAge AND today < Vaccine[?v].RecAge1").
r(Vaccines, a3, "Vaccine[?v].Status = 'Due' WHEN today >= Vaccine[?v].RecAge1 AND today < Vaccine[?v].RecAge2").
r(Vaccines, a4, "Vaccine[?v].Status = 'Current' WHEN today < Vaccine[?v].MinAge").
r(Vaccines, a5, "Vaccine[?v].Status = 'Complete' WHEN ?v!Complete").
r(Vaccines, a6, "Vaccine[?v].Status = 'None'").
r(Vaccines, a7, "Vaccine[?v].Dose = ?v!Dose").
r(Vaccines, a8, "Vaccine[?v].MinAge = ?v!MinAge").
r(Vaccines, a9, "Vaccine[?v].RecAge1 = ?v!RecAge1").
r(Vaccines, a10, "Vaccine[?v].RecAge2 = ?v!RecAge2").
r(Vaccines, a11, "Birthdate = date(1977, 7, 7)").
r(Vaccines, a12, "today = date(2005,7, 7)").

r(TWINRIX, a0, "inherit from vaccines").
r( TWINRIX, a1,
  "Dose = 1 AND MinAge = today AND RecAge1 = today AND RecAge2 = today
  WHEN
  Age >= 18 years AND Doses = 0" ).
r( TWINRIX, a2,
  "Dose = 2 AND MinAge = ?x AND RecAge1 = ?x AND RecAge2 = ?x
  WHEN
  Age >= 18 years and Doses = 1 and ?x = vaccination[1].date + 1 month").
r( TWINRIX, a3,
  "Dose = 3 AND MinAge = vaccination[1].date + 6 months AND RecAge1 = vaccination[1].date + 6 months AND RecAge2 = vaccination[1].date + 6 months
  WHEN
  Age >= 18 years AND Doses = 2").
r( TWINRIX, a4,
  "Age = today - BirthDate").
r(TWINRIX, a6, "Doses = 0").
r(TWINRIX, a7, "vaccination[1].date = date(2005, 9, 7) ").
r(TWINRIX, a8, "vaccination[2].date = date(2005, 10, 7)").
r(TWINRIX, a9, "vaccination[3].date = 0").
r(TWINRIX, a10, "complete = 'false'").

r(IPV, a0, "inherit from vaccines").
r(IPV, a1, "Complete WHEN Doses >= 5").
r(IPV, a2, "Complete WHEN Doses = 3 AND vaccination[3].date > Birthdate + 4 years").
r(IPV, a3, "Complete WHEN Doses = 4 AND vaccination[4].date > Birthdate + 4 years").
r(IPV, a4, "Dose = NA AND MinAge = NA and RecAge1 = NA AND RecAge2 = NA WHEN Complete").
r(IPV, a5, "Dose = 1 AND MinAge = birthdate + 6 weeks AND RecAge1 = Birthdate AND RecAge2 = Birthdate
WHEN
Doses = 0").
r(IPV, a6, "Dose = 2 AND MinAge = maximum(Birthdate + 10 weeks, vaccination[1].date + 4 weeks) AND RecAge1 = maximum(Birthdate + 4 months, vaccination[1].date + 4 weeks) AND RecAge2 = maximum(Birthdate + 4 months, vaccination[1].date + 4 weeks)
WHEN
Doses = 1").
r(IPV, a6a, "Dose = 2 AND MinAge = maximum(Birthdate, vaccination[1].date) AND RecAge1 = maximum(Birthdate, vaccination[1].date) AND RecAge2 = maximum(Birthdate, vaccination[1].date)
WHEN
Doses = 1").
r(IPV, a7, "Dose = 3 AND MinAge = maximum(Birthdate + 14 weeks, vaccination[2].date + 4 weeks) AND RecAge1 = maximum(Birthdate + 6 months, vaccination[2].date + 4 weeks) AND RecAge2 = maximum(Birthdate + 18 months, vaccination[2].date + 4 weeks)
WHEN
Doses = 2").
r(IPV, a8, "Dose = 4 AND MinAge = maximum(Birthdate + 18 weeks, vaccination[3].date + 4 weeks) AND RecAge1 = maximum(Birthdate + 4 months, vaccination[3].date + 4 years) AND RecAge2 = maximum(Birthdate + 4 months, vaccination[3].date + 6 years)
WHEN
Doses = 3").
r(IPV, a9, "Doses = 0").
r(IPV, a10, "vaccination[1].date = 0").
r(IPV, a11, "vaccination[2].date = 0").
r(IPV, a12, "vaccination[3].date = 0").
r(IPV, a13, "vaccination[4].date = 0").
r(IPV, a14, "vaccination[5].date = 0").


q(Vaccines, "find vaccine['twinrix'].dose and vaccine['twinrix'].status and vaccine['twinrix'].minage and vaccine['twinrix'].recage1 and vaccine['twinrix'].recage2",
               [1, none, "2005-7-7", "2005-7-7", "2005-7-7"] ).
q(Vaccines, "find vaccine['ipv'].dose and vaccine['ipv'].status and vaccine['ipv'].minage and vaccine['ipv'].recage1 and vaccine['ipv'].recage2",
               [1, Behind, "1977-8-18", "1977-7-7", "1977-7-7"] ).

%-----------------------------------------------------------------
case(23, [ rulesets = [r23] ]).

r(r23, a1, "element{?x, [?x | ?]}").
r(r23, a2, "element{?x, [? | ?y]} WHEN element{?x, ?y}").
r(r23, a3, "primary when element{ color, ['red','blue','yellow'] }").
r(r23, a4, "colors = FINDALL( ?x, element{?x, ['red', 'blue', 'yellow']})").
r(r23, a5, "c = ?x WHEN element{?x, ['red', 'blue', 'yellow']}").
r(r23, a6, "cs = FINDALL(?x, c = ?x)").

q(r23, "find primary when color = 'blue'", true).


%-----------------------------------------------------------------
case(24, [ rulesets = [r24] ]).

r(r24, a1, "price = part[?x].price WHEN ?x = part").
r(r24, a0, "note = part[?x].note WHEN ?x = part").
r(r24, a2, "part[?x].price = part[?x].quantity * part[?x].unit_price * part[?x].discount").

r(r24, a3, "part['widget'].discount = 0.8 when part['widget'].quantity >= part['widget'].discount_quantity").
r(r24, a4, "part['widget'].unit_price = 10").
r(r24, a5, "part['widget'].discount_quantity = 10").

r(r24, a6, "part['gizmo'].discount = 0.9 when part['gizmo'].quantity >= part['gizmo'].discount_quantity").
r(r24, a7, "part['gizmo'].unit_price = 20").
r(r24, a8, "part['gizmo'].discount_quantity = 20").

r(r24, a9, "part[?x].discount = 1.0").
r(r24, a10, "part[?x].quantity = quantity").

r(r24, a11, "part[?part].note = 'You saved ' & ?savings & ' buying ' & ?part & 's' WHEN part[?part].discount < 1.0 AND ?savings = part[?part].quantity * part[?part].unit_price - part[?part].price").
r(r24, a12, "part[?p].note = 'You need to purchase at least ' & part[?p].discount_quantity & ' ' & ?p & 's to get a discount.'").

q(r24, "FIND price and note when part = 'gizmo' and quantity = 10",
   [200, "You need to purchase at least 20 gizmos to get a discount."]).
q(r24, "FIND price and note when part = 'widget' and quantity = 10",
   [80, "You saved 20 buying widgets"]).

%-----------------------------------------------------------------
case(25, [ rulesets = [r25] ]).

r(r25, a1, "status = \"Dose \" & dose & \" on or after \" & next_date WHEN count < 3").
r(r25, a2, "dose = count + 1").
r(r25, a3, "next_date = birthdate + 6 months").

q(r25, "FIND status when count = 0 and birthdate = datetime(2005, 9, 1, 0, 0, 0)",
    "Dose 1 on or after 2006-3-1").

%-----------------------------------------------------------------
case(26, [ rulesets = [r26] ]).

d(r26, pl, [
  [Income, Outgo, Net],
  [January, February, March],
  [100, 150, 150],
  [120, 130, 140],
  [-20, 20, 10]] ).

r(r26, a1, "profit[?m] = 'Good' WHEN pl['Income', ?m] > pl['Outgo', ?m]").
r(r26, a2, "profit[?m] = 'Bad' WHEN pl['Income', ?m] <= pl['Outgo', ?m]").
r(r26, a3, "trend[?m] = 'Good' WHEN ?mm = prior(pl.headers[2], ?m) AND pl['Net', ?m] > pl['Net', ?mm]").
r(r26, a4, "trend[?m] = 'Bad' WHEN ?mm = prior(pl.headers[2], ?m) AND pl['Net', ?m] <= pl['Net', ?mm]").
r(r26, a5, "profits = FINDALL( [?m, ?x], profit[?m] = ?x )").
r(r26, a6, "row[?i] = pl[?i, *]").
r(r26, a7, "col[?i] = pl[*, ?i]").
r(r26, a8, "rows = FINDALL( ?r, pl[?, *] = ?r )").
r(r26, a9, "c = RANGE( pl['Income', 'January'] )").
r(r26, a10, "d = RANGE( pl['Income', *] )").

q(r26, "FIND profit['February']", Good).
q(r26, "FIND trend['February']", Good).
q(r26, "FIND profit['March']", Good).
q(r26, "FIND trend['March']", Bad).
q(r26, "FIND profits", [['February', 'good'], ['March', 'good'], ['January', 'bad']]).
q(r26, "FIND row['Income']", [100, 150, 150]).
q(r26, "FIND row['Outgo']", [120, 130, 140]).
q(r26, "FIND col['February']", [150, 130, 20]).
q(r26, "FIND rows", [[100, 150, 150], [120, 130, 140], [-20, 20, 10]]).
q(r26, "FIND c", "d1").
q(r26, "FIND d", "d1:d1").

%-----------------------------------------------------------------
case(27, [ rulesets = [r27] ]).

r(r27, a1, "price['gizmo'] = 3").
r(r27, a2, "price['widget'] = 4").
r(r27, a3, "prices = FINDALL([?part, ?price], price[?part] = ?price)").
r(r27, a4, "today = today").
r(r27, a5, "weekday = WEEKDAY(today)").
r(r27, a6, "truth['gizmo']").
r(r27, a7, "truth['widget'] = 'false'").
r(r27, a8, "truth['thing']").
r(r27, a9, "truth['object'] = 'false'").
r(r27, a10, "truths = FINDALL( ?part, truth[?part] )").
r(r27, a11, "falsehoods = FINDALL( ?part, truth[?part] = 'false' )").
r(r27, a12, "a[1] = 11").
r(r27, a13, "a[3] = 33").
r(r27, a14, "a[2] = 22").
r(r27, a15, "as = FINDALL( [?i, ?ii], a[?i] = ?ii )").
r(r27, a15, "ssas = SORT( FINDALL( [?i, ?ii], a[?i] = ?ii ) )").
r(r27, a16, "sas = SORT( as )").

q(r27, "FIND prices", [[gizmo, 3], [widget, 4]]).
q(r27, "FIND truths", [gizmo, thing]).
q(r27, "FIND falsehoods", [widget, object]).
q(r27, "FIND as", [[1, 11], [3, 33], [2, 22]]).
q(r27, "FIND sas", [[1, 11], [2, 22], [3, 33]]).
q(r7, "FIND ssas", [[1, 11], [2, 22], [3, 33]]).

%-----------------------------------------------------------------
case(28, [ rulesets = [r28] ]).

r(r28, a1, "cold WHEN temp < 50").
r(r28, a2, "hot WHEN temp > 70").
r(r28, a3, "sweatshirt WHEN cold").
r(r28, a4, "teeshirt AND shorts WHEN hot").
r(r28, a5, "shirt WHEN not hot AND not cold").

q(r28, "FIND sweatshirt and teeshirt and shirt and shorts WHEN temp = 40",
   [true, false, false, false]).
q(r28, "FIND sweatshirt and teeshirt and shirt and shorts WHEN temp = 60",
   [false, false, true, false]).
q(r28, "FIND sweatshirt and teeshirt and shirt and shorts WHEN temp = 80",
   [false, true, false, true]).

%-----------------------------------------------------------------
case(29, [ rulesets = [r29] ]).

r(r29, a1, "a = 1").

q(r29, "FIND a", 1).

%-----------------------------------------------------------------
case(30, [ rulesets = [r30] ]).

r(r30, a1, "part['bike'].contains = [ ['wheel',2], ['pedal',2]]").
r(r30, a2, "part['wheel'].contains = [ ['spoke',36], ['rim',1], ['hub',1]]").
r(r30, a2a, "part[?x].contains = '[]'").
r(r30, a3, "contains{?a, ?a, ?p} WHEN
                  part[?a].contains = ?l AND mimber{ [?p, ?q], ?l }").
r(r30, a4, "contains{?a1, ?a, ?p} WHEN
                  part[?a1].contains = ?l and mimber{ [?a2, ?q], ?l } and contains{?a2, ?a, ?p}").
r(r30, a5, "contents = FINDALL( [?a, ?p], contains{'bike', ?a, ?p} )").
r(r30, a6, "thing[?i] = ?i WHEN mimber{ ?i, ['red', 'blue', 'yellow'] }").
r(r30, a7, "things = FINDALL( ?i, thing[?i] = ?i )").
r(r30, a8, "flatcontents = CONCATENATE( contents )").

q(r30, "FIND contents", [[bike, wheel], [bike, pedal], [wheel, spoke], [wheel, rim], [wheel, hub]]).
q(r30, "FIND things", [red, blue, yellow]).
q(r30, "FIND flatcontents", [bike, wheel, bike, pedal, wheel, spoke, wheel, rim, wheel, hub]).


%-----------------------------------------------------------------
case(31, [ rulesets = [r31] ]).


r(r31, a1, "assembly['Door Unit'].property['height'] = 72").
r(r31, a2, "assembly['Door Unit'].property['width'] = 56").
r(r31, a5, "assembly['Door Unit'].contains = [ ['Frame', 1] ]").
r(r31, a8, "assembly['Frame'].cost =
      10 * assembly['Door Unit'].property['height'] *
      assembly['Door Unit'].property['width'] / 144").

q(r31, "FIND assembly['frame'].cost", 280).

%------------------------------------------------------------------
case(32, [ rulesets = [r32, mmr32] ]).

r(r32, a1, "data['birthdate'] = date(2002, 1, 15)").
r(r32, a2, "status[?v, ?n, ?, ?d] = ['X', 'Before min. age'] WHEN
   ?d < data['birthdate'] + ?v ! table[?n, 'Minimum Age']" ).
r(r32, a3, "status[?v, ?n, ?d1, ?d2] = ['X', 'Before min. interval'] WHEN
   ?n > 1 AND
   ?d2 < ?d1 + ?v ! table[?n, 'Minimum Interval']" ).
r(r32, a4, "status[?v, ?n, ?, ?d] = ['OK', 'Minimum interval'] WHEN
   ?d < data['birthdate'] + ?v ! table[?n, 'Recommended 1']" ).
r(r32, a5, "status[?v, ?n, ?, ?d] = ['OK', 'Recommended age range'] WHEN
   ?d =< data['birthdate'] + ?v ! table[?n, 'Recommended 2']" ).
r(r32, a6, "status[?v, ?n, ?, ?d] = ['OK', 'After recommended range'] WHEN
   ?d > data['birthdate'] + ?v ! table[?n, 'Recommended 2']" ).
r(r32, a7, "row[?i] = FINDALL(?v, mmr32 ! table[?i, ?x] = ?v)").

d(mmr32, table, [
  [1, 2],
  ['Minimum Age', 'Minimum Interval',' Recommended 1', 'Recommended 2', 'Grace Period'],
  [`12 months`, none, `12 months`, `15 months`, none],
  [`13 months`, `4 weeks`, `4 years`, `6 years`, `4 days`]] ).

q(r32, "FIND status['mmr32', 1, 'na', date(2002,11,15)]", [X, 'Before min. age']).
q(r32, "FIND row[2]", ["13 months", "4 weeks", "4 years", "6 years", "4 days"]).

%------------------------------------------------------------------
case(33, [ rulesets = [r33] ]).

r(r33, a1, "x = MAXIMUM( ['a', 'b', 'c', 'd'] )").
r(r33, a2, "y = MINIMUM( ['a', 'b', 'c', 'd'] )").
r(r33, a3, "vs = [ [date(2002, 10, 15), 'MMR'], [date(2003, 11, 22), 'Varicella'] ]").
r(r33, a4, "mvs = MAXIMUM( vs )").
r(r33, a6, "lvs = [ date(2002,10,15), date(2003,1,12), date(2003,2,5), date(2003,11,6) ]").
r(r33, a7, "bef = before( lvs, qd )").
r(r33, a8, "aft = after( lvs, qd )").

q(r33, "FIND x", d).
q(r33, "FIND y", a).
q(r33, "FIND mvs", ["2003-11-22", Varicella]).
q(r33, "FIND bef WHEN qd = date(2003,10,3)", "2003-2-5").
q(r33, "FIND aft WHEN qd = date(2002,11,3)", "2003-1-12").

%------------------------------------------------------------------
case(34, [ rulesets = [r34] ]).

r(r34, a1, "region['east'].pl[2004,'income'] = 100").
r(r34, a2, "region['east'].pl[2004,'outgo'] = 110").
r(r34, a3, "region['east'].pl[2005,'income'] = 120").
r(r34, a4, "region['east'].pl[2005,'outgo'] = 130").
r(r34, a5, "region['west'].pl[2004,'income'] = 140").
r(r34, a6, "region['west'].pl[2004,'outgo'] = 150").
r(r34, a7, "region['west'].pl[2005,'income'] = 160").
r(r34, a8, "region['west'].pl[2005,'outgo'] = 170").
r(r34, a9, "ry[?r,?y] = region[?r].pl[?y, *]").
r(r34, a10, "yt[?y,?t] = region[*].pl[?y, ?t]").
r(r34, a11, "r[?r,?y] = region[?r].pl[?y, *]").

q(r34, "FIND ry['east', 2004]", [100, 110]).
q(r34, "FIND yt[2004, 'income']", [100, 140]).
q(r34, "FIND r['west', 2005]", [160, 170]).

%------------------------------------------------------------------
case(35, [ rulesets = [r35] ]).

r(r35, a1, "abc = [1, 2, 3]").
r(r35, a2, "a = 1").
r(r35, a3, "b = 2").
r(r35, a4, "c = 3").
r(r35, a5, "cba = [c, 2 * b, [c + a + b, 4], 3 * a] WHEN abc = [a, b, c]").
r(r35, a6, "q = [?a, ?b, ?c] WHEN ?a = a AND ?b = b AND ?c = c").
r(r35, a7, "aa = 2 * a").
r(r35, a8, "duh when is_ruleset('r35')").
r(r35, a9, "d").
r(r35, a10, "e WHEN d").

q(r35, "FIND abc", [1, 2, 3]).
q(r35, "FIND cba", [3, 4, [6, 4], 3]).
q(r35, "FIND q", [1, 2, 3]).
q(r35, "FIND aa", 2).
q(r35, "FIND duh", true).
q(r35, "FIND e", true).

%------------------------------------------------------------------
case(36, [ rulesets = [r36, r36b, r36c, r36d] ]).

r(r36, a1, "a[?m] = ?m:a").
r(r36, a2, "as = FINDALL( ?x, (mimber{?i, ['r36b', 'r36c']} AND a[?i] = ?x) )").

r(r36b, a1, "a = 1").
r(r36c, a1, "a = 2").
r(r36d, a1, "a = 3").

q(r36, "FIND as", [1,2]).

%------------------------------------------------------------------
case(37, [ rulesets = [r37, r37mmr] ]).

d(r37, vaccines, [
  [1, 2, 3],
  ['Vaccine', 'Type'],
  [HepA, other],
  [r37MMR, 'live virus'],
  [Varicella, 'live virus']] ).

r(r37, a1, "plan[?v] = ?v:plan WHEN IS_RULESET(?v)").
r(r37, a1a, "plan[?v] = 'no'").
r(r37, a2, "plans = FINDALL( ?p, (vaccines[?, 'vaccine'] = ?v AND plan[?v] = ?p ) )").

r(r37mmr, a1, "plan = ['a','b','c']").

q(r37, "FIND plans", [no, [a,b,c], no]).

%------------------------------------------------------------------
case(38, [ rulesets = [r38] ]).

r(r38, a1, "bdate = date(1946,2,24)").
r(r38, a2, "today = date(2005,12,4)").
r(r38, a3, "bday = date(?y, ?m, ?d) WHEN ?m = MONTH(bdate) AND ?d = DAY(bdate) AND ?y = YEAR(today)").
r(r38, a4, "bmon = ?m WHEN ?m = MONTH(bdate)").
r(r38, a5, "sib[?x, ?y] WHEN mf[?x, Mother] = mf[?y, Mother] AND ?x <> ?y").

q(r38, "FIND bmon", 2).
q(r38, "FIND bday", "2005-2-24").

%------------------------------------------------------------------
case(39, [ rulesets = [r39] ]).

r(r39, a0, "RuleSetLocale['language'] = 'French'").
r(r39, a1, "a = 3 QUAND b > 3").
r(r39, a3, "aday = date(2005;12;5)").
r(r39, a2, "today = 'A day is ' & JOUR_SEMAINE(aday) & ' the ' & aday").
r(r39, a4, "b = 3 QUAND EST_CONNU(3)").
r(r39, a5, "c = ?x QUAND ?x = PREMIER( ['red'; 'blue'] )").
r(r39, a6, "data['mary'; 'age'] = 46 années 1 jour").
r(r39, a7, "data['dennis'; 'age'] = 59 années 9 mois 20 jour").
r(r39, a8, "data['mary'; 'gender'] = 'female'").
r(r39, a9, "gm = data['mary'; 'gender']").

q(r39, "CHERCHE today", "A day is Lundi the 2005-12-5").
q(r39, "CHERCHE c", red).
q(r39, "CHERCHE gm", female).
q(r39, "CHERCHE data['mary'; 'gender']", female).

%----------------------------------------------------------------------
case(40, [ rulesets = [r40] ]).

r(r40, a0, "RuleSetLocale['language'] = 'French'").
r(r40, a1, "x['dennis';'age'] = 59").
r(r40, a2, "da = x['dennis';'age']").

q(r40, "FIND da", 59).

%----------------------------------------------------------------------
case(41, [ rulesets = [r41] ]).

r(r41, a0, "vaccination_comments = FINDALL( [?range, ?status], 
   ( [?v, ?dose, ?date, ?, ?status] = member( history) AND
     vaccination[?i] = [?v, ?date] AND
     ?range = RANGE( raw_vaccination[?i] ) ) )" ).
r(r41, d0, "vaccination_comments = FINDALL( [?range, ?status], 
     ?range = RANGE( raw_vaccination[?i] ) )" ).
r(r41, b0, "vaccination_comments = FINDALL( [?range, ?status], [?v, ?dose, ?date, ?, ?status] = member(history) )" ).
r(r41, c0, "vaccination_comments = FINDALL( [?range, ?status], 
   ( [?v, ?dose, ?date, ?, ?status] = member( history) AND
     vaccination[?i] = [?v, ?date] ) )" ).
r(r41, a1, "colors = FINDALL( [?n, ?c] , ( [?n, ?c] = member( rby ) AND ?n > 1 ) )").
r(r41, a2, "rby = [[1,'red'], [2,'blue'], [3,'yellow']]").
r(r41, a3, "y = FINDALL( ?a, ?a = MEMBER(['red', 'blue', 'green']) )").
r(r41, a4, "yes").

q(r41, "FIND colors", [[2,blue], [3,yellow]]).
q(r41, "FIND y", [red, blue, green]).

%----------------------------------------------------------------------
case(42, [ rulesets = [r42] ]).

r(r42, a1, "a WHEN 'orange' = MEMBER( ['cyan', 'fushia', 'orange'] )").
r(r42, a2, "b WHEN mimber{ ?x, ['cyan', 'fushia', 'orange'] } AND ?x = 'orange'").
r(r42, a3, "c WHEN IS_RULESET('r42')").
r(r42, a3, "d WHEN IS_RULESET('nope')").

q(r42, "FIND a", true).
q(r42, "FIND b", true).
q(r42, "FIND c", true).
q(r42, "FIND d", false).

%----------------------------------------------------------------------
case(43, [ rulesets = [r43] ]).

d(r43, vehicle, [
  [1,2,3,4,5,6],
  [Type, Make],
  [truck, ford],
  [car, honda],
  [truck, dodge],
  [car, ford],
  [truck, chevy],
  [car, chevy] ] ).

r(r43, a1, "trucks = FINDALL( ?m, (vehicle[?i,'type'] = truck AND vehicle[?i,'make'] = ?m) )").
r(r43, a2, "truckcar{ '[]', '[]' }").
r(r43, a3, "truckcar{ [?tm | ?tms], [?tm | ?tcms] } WHEN
                vehicle[?i, *] = ['car', ?tm] AND
                truckcar{?tms, ?tcms}").
r(r43, a4, "truckcar{ [? | ?tms], ?tcms } WHEN truckcar{ ?tms, ?tcms }").
r(r43, a5, "tcs =  ?x WHEN truckcar{ trucks, ?x }").
r(r43, a6, "stuff = APPEND( ['car', 'cup', 'calendar'], trucks )").
r(r43, a7, "things[?t] = FINDALL( ?m, (vehicle[?i,type] = ?t AND vehicle[?i,make] = ?m) )").
r(r43, a8, "torc = UNION( things['car'], things['truck'] )").
r(r43, a9, "tandc = INTERSECTION( things['car'], things['truck'] )").
r(r43, a10, "tdiffc = DIFFERENCE( things['car'], things['truck'] )").
r(r43, a11, "tissc = IS_SUBSET( things['car'], things['truck'] )").
r(r43, a12, "what = REVERSE( APPEND( APPEND(torc, tandc ), tdiffc ) )").
r(r43, a13, "manytrucks = CONCATENATE( [ trucks, trucks, trucks, trucks ] )").
r(r43, a14, "numbers = CONCATENATE( [ [1,2], [3,4], [5,6] ] )").
r(r43, a15, "things = things[ 'vehicle' ]").
r(r43, a16, "car[?i] = ?car WHEN vehicle[?i, *] = ['car', ?car]").
r(r43, a17, "cars = FINDALL( ?car, car[?i] = ?car )").

q(r43, "FIND cars", [honda, ford, chevy]).


%----------------------------------------------------------------------
case(44, [ rulesets = [r44] ]).

r(r44, a1, "a = b[ c, e ].f[g] WHEN h[i] = j[k, l]").
r(r44, a11, "a3 = b[ c, e ]").
r(r44, a14, "a4 = b[ c, e ].f[g]").
r(r44, a15, "a5 = b[ c, e ].f[4]").
r(r44, a13, "a1 = b[ c, e ] WHEN h[i] = j[k, l]").
r(r44, a12, "a2 = b[c]").
r(r44, a2, "i = 1 and k = 2 and l = 3").
r(r44, a3, "h[1] = 3 and j[2, 3] = 3").
r(r44, a4, "g = 4 and e = 5 and c = 6").
r(r44, a5, "d[5] = 7").
r(r44, a6, "b[6,5].f[4] = 'Yippee'").
r(r44, a7, "b[6,5] = 'notbad'").
r(r44, a8, "b[6] = 'minimal'").

q(r44, "FIND a", Yippee).
q(r44, "FIND a4", Yippee).
q(r44, "FIND a5", Yippee).
q(r44, "FIND a1", notbad).
q(r44, "FIND a3", notbad).
q(r44, "FIND a2", minimal).

%----------------------------------------------------------------------
case(45, [ rulesets = [r45, r45a] ]).


d(r45a, isa, [
  [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17],
  [Class, Subset],
  [food, vegetable],
  [food, fruit],
  [fruit, apple],
  [fruit, orange],
  [fruit, pear],
  [apple, macintosh],
  [apple, golden],
  [vegetable, carrot],
  [vegetable, bean],
  [vegetable, tomato],
  [bean, string],
  [bean, wax],
  [vegetable, 'Brussel sprouts'],
  [string, cotton],
  [string, rayon],
  [bean, green],
  [hardware, nail] ] ).


r(r45a, a0, "edible = FINDALL( ?x, contains{'food', ?x} )").
r(r45a, a3, "contains{?x, ?y} WHEN isa[?, *] = [?x, ?y]").
r(r45a, a4, "contains{?x, ?y} WHEN isa[?, *] = [?x, ?z] AND contains{?z, ?y}").
r(r45a, a5, "quantity = COUNT( isa[*, 'Class'] )").
r(r45a, a6, "quantity[?x] = COUNT( FINDALL( ?i, isa[?i, 'Class'] = ?x) )").
r(r45a, a7, "issa[?x,?y] = isa[?x,?y]").

r(r45, a0, "quantity[?x] = COUNT( FINDALL( [?i,?a], ?x:issa[?i, 'Class'] = ?a ) )").
r(r45, a1, "is_edible[?x] WHEN r45a:contains{'food', ?x}").

q(r45, "FIND quantity['r45a']", 17).
q(r45, "using r45a FIND quantity", 17).
q(r45, "using r45a FIND quantity['fruit']", 3).
q(r45, "using r45a FIND edible", [vegetable, fruit, carrot, bean, tomato, 'Brussel sprouts', string, wax, green, cotton, rayon, apple, orange, pear, macintosh, golden]).
q(r45, "FIND is_edible['apple']", true).
q(r45, "FIND is_edible['nail']", false).

%----------------------------------------------------------------------
case(46, [ rulesets = [r46] ]).


r(r46, a0, "late_startx WHEN first_date['DT'] <= birthdate + 7 OR full_count['DT'] = 0 AND age <> 7 years").
r(r46, a1, "late_startx WHEN  birthdate + 7 <= first_date['DT']").
r(r46, a2, "late_startx WHEN first_date['DT'] <= birthdate + 7 years").
r(r46, a3, "x WHEN date(2004,3,3) > \"\"").
r(r46, a4, "late_start WHEN
   first_date['DT'] >= birthdate + 7 years OR
   ( full_count['DT'] = 0 AND age >= 6 years )").

r(r46, a5, "first_date['DT'] = date(2000,12,23)").
r(r46, a6, "birthdate = date(2000,10,23)").
r(r46, a5, "full_count['DT'] = 0").
r(r46, a6, "age = date(2003,10,22) - birthdate").

q(r46, "FIND x", false).
q(r46, "FIND late_start", false).

%-------------------------------------------------------------------------
% raising errors
%
case(47, [ rulesets = [r47] ]).

r(r47, a0, "x = 'a' WHEN 'b' = 'a'").
r(r47, a1, "x = 'b' WHEN NOT x = 'a'").
r(r47, a2, "z = BEFORE( y, 3 )").
r(r47, a3, "z = msgbox('no value for z')").
r(r47, a4, "letter['a'].number[1] = 'a1'").
r(r47, a5, "letter['a'].number[2] = 'a2'").
r(r47, a6, "letter['b'].number[1] = 'b1'").
r(r47, a7, "letter['b'].number[2] = 'b1'").
r(r47, a8, "letter['a'] = 'a'").
r(r47, a8, "letter['b'] = 'b'").
r(r47, a8, "letter['c'] = 'c'").
r(r47, a10, "numbers[?a] = FINDALL( [?n, ?x], letter[?a].number[?n] = ?x ) WHEN EXISTS(letter[?a].number[?])").
r(r47, a11, "numbers[?a] = '[]' WHEN NOT EXISTS(letter[?a].number[?])").
r(r47, a12, "numbers = FINDALL( [?a,?ns], (letter[?a] = ?a AND numbers[?a] = ?ns) )").

q(r47, "FIND numbers", [[a, [[1, a1], [2, a2]]], [b, [[1, b1], [2, b1]]], [c, '[]']] ).

%-------------------------------------------------------------------------
case(48, [ rulesets = [r48] ]).

d(r48, rawvac2, [
  [1, 2, 3],
  ['Vaccine', 'Date'],
  [mmr, 2002],
  [DTaP, 2003],
  [mmr, 2004]] ).

r(r48, a0, "rawvac[1,'vaccine'] = 'mmr'").
r(r48, a1, "rawvac[1,'date'] = 2002").
r(r48, a2, "rawvac[2,'vaccine'] = 'DTaP'").
r(r48, a3, "rawvac[2,'date'] = 2003").
r(r48, a4, "rawvac[3,'vaccine'] = 'mmr'").
r(r48, a5, "rawvac[3,'date'] = 2004").
r(r48, a6, "v[1] = 'mmr'").
r(r48, a7, "v[2] = 'DTaP'").
r(r48, a8, "vac[?i] = [?v, ?d] WHEN rawvac[?i, *] = [?v, ?d]").
r(r48, a9, "hist = FINDALL(?d, (v[?i] = ?v AND vac[?j] = [?v, ?d]) )").
r(r48, a10, "vac2[?i] = [?v, ?d] WHEN rawvac2[?i, *] = [?v, ?d]").
r(r48, a11, "hist2 = FINDALL(?d, (v[?i] = ?v AND vac2[?j] = [?v, ?d]) )").
r(r48, a12, "vac2s = FINDALL(?x, vac2[?] = ?x)").
r(r48, a13, "vacs = FINDALL( ?v, rawvac[?, vaccine] = ?v )").
r(r48, a14, "vplan['DTaP'] = 'DTaP plan' ").
r(r48, a14, "vplan['mmr'] = 'mmr plan' ").
r(r48, a15, "plans= FINDALL(?p, ( MEMBER(vacs) = ?v AND vplan[?v] = ?p ))").

q(r48, "FIND vac[2]", [DTaP, 2003]).
q(r48, "FIND vac[?x]", [mmr, 2002]).


%--------------------------------------------------------------------------------------
case(49, [ rulesets = [r49] ]).

r(r49, A34, " WHEN duration <= OverTime THEN price = duration * ~get rate below~ rate").
r(r49, A35, " WHEN duration > overtime THEN price = rate * duration + Surcharge * (duration - Overtime)").
r(r49, A36, " WHEN weekend THEN rate = 0.05").
r(r49, A37, " WHEN weekday AND daytime THEN rate = 0.10").
r(r49, A38, " WHEN weekday AND nighttime THEN rate = 0.07").
r(r49, A39, " WHEN day ='Sunday' OR day = 'Saturday' THEN weekend").
r(r49, A40, " WHEN day = 'Friday' AND time > FridayWeekendTime THEN weekend").
r(r49, A41, " WHEN NOT weekend THEN weekday").
r(r49, A42, " WHEN time >= 2000 OR time =< 500 THEN nighttime").
r(r49, A43, " WHEN NOT nighttime THEN daytime").
r(r49, A44, "Surcharge = 0.7").
r(r49, A45, "OverTime = 9").
r(r49, A46, "FridayWeekendTime = 2350").

q(r49, "FIND price WHEN day = 'Saturday' AND time = 2200 and duration = 11", 1.95).
q(r49, "FIND price WHEN day = 'wednesday' AND time = 2200 and duration = 11", 2.17).

%-----------------------------------------------------------------------------------------
case(50, [ rulesets = [r50] ]).

r(r50, a1, "a = 1 AND b = 1 WHEN x = 1").
r(r50, a2, "a = 2 AND c = 2 WHEN x > 0").


%-----------------------------------------------------------------------------------------
case(51, [ rulesets = [r51, system] ]).

/*
r(system, d1, "database[gene].type = MySQL").
r(system, d2, "database[gene].user = root").
r(system, d3, "database[gene].password = katoollie").
r(r51, a1, "family[?x] = SQL(gene, \"SELECT name, midname, surname from people where surname = '\" & ?x & \"'\") ").
r(r51, a2, "midname = ?x WHEN [.firstname, ?x, surname] = MEMBER( family[.surname] )").
r(r51, a3, "sibling[?x] = ?y WHEN parents[?x] = parents[?y] AND ?x <> ?y").
r(r51, a4, "fullname[?x] = SQL(gene, \"SELECT name, midname, surname from people WHERE pid = '\" & ?x & \"'\") ").
r(r51, a5, "parents =  SQL(gene, \"SELECT pid, mother, father from people WHERE mother <> 0\") ").
r(r51, a5, "parents[?x] =  [?m, ?f] WHEN [?x, ?m, ?f] = MEMBER( parents )").
r(r51, a6, `.pid = SQL(gene,  "SELECT pid from people where surname = '" & surname & "' AND name = '" & name & "'") `).
r(r51, a7, "siblings1 = FINDALL( [?s1, ?s2], sibling[?s1] = ?s2 ) ").
r(r51, a8, "siblings2 = FINDALL( [?s1, ?s2], ( [?s1, ?m, ?f] = MEMBER(parents) AND [?s2, ?m, ?f] = MEMBER(parents) AND ?s1 <> ?s2 ) ) ").
r(r51, a9, "siblings[?x] = FINDALL( ?y, ( parents[?x] = parents[?y] AND ?x <> ?y ) ) ").
r(r51, a10, "siblings2[?x] = FINDALL( ?y, sibling[?x] = ?y ) ").

q(r51, "FIND family[fox]", [["Joe", "Fox", "Fox"], ["Cindy", "Wren", "Fox"], ["William", "Wren", "Fox"], ["Charlie", "Moose", "Fox"], ["Samantha", "Moose", "Fox"], ["Peter", "Moose", "Fox"]]).
q(r51, "FIND midname WHEN surname = fox AND firstname = Joe", "Fox").
q(r51, "FIND fullname[24]", [["Wanda", "Hawk", "Squirrel"]]).
q(r51, "FIND sibling[\"7\"]", "8").
q(r51, "FIND siblings1", ?x).
q(r51, "FIND siblings2", ?x).
q(r51, "FIND parents", ?x).
q(r51, "FIND siblings2[\"9\"]", ?x).
*/

%-----------------------------------------------------------------------------------------
case(52, [ rulesets = [r52] ]).

r(r52, a1, "digits = [1,2,3]").
r(r52, a2, "answer = ?s WHEN ?s = [?a, ?b, ?c, ?d] AND ?s = [?, 3, 2, ?] AND [?a, ?b, ?c] = PERMUTE(digits) AND [?b, ?c, ?d] = PERMUTE(digits)").
r(r52, a3, "something = FINDALL( ?x, ( ?x = MEMBER( digits ) AND ONETIME( ?x = 2 ) ) )").
r(r52, a4, "odds = [1,3]").

q(r52, "FIND answer", [1, 3, 2, 1]).
q(r52, "FIND something", [2]).

%-----------------------------------------------------------------------------------------
case(53, [ rulesets = [r53] ]).

d(r53, pl, [
  [Income, Outgo, Net],
  [January, February, March],
  [100, 150, 150],
  [120, 130, 140],
  [-20, 20, 10]] ).

r(r53, a1, "jans = FINDALL( ?x, pl[?, 'January'] = ?x )").

q(r53, "FIND jans", [100, 120, -20]).

%-----------------------------------------------------------------------------------------
case(54, [ rulesets = [r54] ]).

r(r54, a1, "a1 = 1 year + 3 months + 11 days").
r(r54, a2, "a2 = 16 months").
r(r54, a3, "bd = date(2000, 1, 1)").
r(r54, a4, "x = maximum( [bd + a1, bd + a2] )").

q(r54, "FIND x", "2001-5-1").

%-----------------------------------------------------------------------------------------
case(55, [ rulesets = [r55] ]).

r(r55, a1, "DennisWithdraw = 12 * Required WHEN DennisFunds > 12 * Required").
r(r55, a2, "DennisWithdraw = DennisFunds").

r(r55, a3, "ShortFall = 12 * Required - DennisWithdraw").

r(r55, a4, "MaryWithdraw = ShortFall WHEN MaryFunds >= ShortFall").
r(r55, a5, "MaryWithdraw = MaryFunds WHEN ShortFall > 0").
r(r55, a6, "MaryWithdraw = 0").

q(r55, "FIND DennisWithdraw WHEN DennisFunds = 0.0419 AND Required = 5251.6", 0.0419).
q(r55, "FIND DennisWithdraw WHEN DennisFunds = - 0.0419 AND Required = 5251.6", -0.0419).

%-----------------------------------------------------------------------------------------
case(56, [ rulesets = [r56] ]).

r(r56, a1, "A = 1 WHEN B = 10").
r(r56, a2, "A = 2 WHEN B = 20").

r(r56, b1, "B = 10 WHEN C = 100").
r(r56, b2, "B = 20 WHEN C = 200").

r(r56, c1, "C = 100 * D").

q(r56, "FIND A WHEN D = 2", 2).

%-----------------------------------------------------------------------------------------
case(57, [ rulesets = [r57, r57a] ]).

d(r57, Animals, [
  [1,2,3,4,5,6],
  [animal, type],
  [lion, wild],
  [pig, farm],
  [dog, pet],
  [tiger, wild],
  [cow, farm],
  [cat, pet] ] ).

r(r57a, r57a_0, "inherit from r57").
r(r57a, r57a_1, "animals = FINDALL(?x, animal[?] = ?x)").
r(r57a, r57a_2, "Animals[?type] = FINDALL( ?x,
     (Animals[?i, 'Animal'] = ?x AND
      Animals[?i, 'Type'] = ?type))").

q(r57, "USING r57a FIND Animals['wild']", [lion, tiger]).

%-----------------------------------------------------------------
case(58, [ rulesets = [r58] ]).

d(r58, pl, [
  [Income, Outgo, Net],
  [January, February, March],
  [100, 150, 150],
  [120, 130, 140],
  [-20, 20, 10]] ).

r(r58, a1, "profit[?m] = 'Good' WHEN pl['Income', ?m] > pl['Outgo', ?m]").
r(r58, a2, "profit[?m] = 'Bad' WHEN pl['Income', ?m] <= pl['Outgo', ?m]").
r(r58, a3, "trend[?m] = 'Good' WHEN ?mm = prior(pl.headers[2], ?m) AND pl['Net', ?m] > pl['Net', ?mm]").
r(r58, a4, "trend[?m] = 'Bad' WHEN ?mm = prior(pl.headers[2], ?m) AND pl['Net', ?m] <= pl['Net', ?mm]").
r(r58, a5, "profits = FINDALL( [?m, ?x], profit[?m] = ?x )").
r(r58, a6, "row[?i] = pl[?i, *]").
r(r58, a7, "col[?i] = pl[*, ?i]").
r(r58, a8, "rows = FINDALL( ?r, pl[?, *] = ?r )").
r(r58, a9, "c = RANGE( pl['Income', 'January'] )").
r(r58, a10, "d = RANGE( pl['Income', *] )").
r(r58, a11, "great_months = FINDALL( ?m, (profit[?m] = 'good' AND trend[?m] = 'good') )").
r(r58, a12, "good_months = FINDALL( ?m, (profit[?m] = 'good' OR trend[?m] = 'good') )").

q(r58, "FIND great_months", [February]).

%-----------------------------------------------------------------
case(59, [ rulesets = [r59] ]).

dd(r59, region(West), [pl(2005, 'budget income', Q1)], 100, "Sheet1!B31").
dd(r59, region(West), [pl(2005, 'budget income', Q2)], 120, "Sheet1!C31").
dd(r59, region(West), [pl(2005, 'budget income', Q3)], 140, "Sheet1!D31").
dd(r59, region(West), [pl(2005, 'budget income', Q4)], 160, "Sheet1!E31").
dd(r59, region(West), [pl(2005, 'budget income', 'Year Total')], 520, "Sheet1!F31").
dd(r59, region(West), [pl(2005, 'Budget Outgo', Q1)], 90, "Sheet1!B32").
dd(r59, region(West), [pl(2005, 'Budget Outgo', Q2)], 95, "Sheet1!C32").
dd(r59, region(West), [pl(2005, 'Budget Outgo', Q3)], 100, "Sheet1!D32").
dd(r59, region(West), [pl(2005, 'Budget Outgo', Q4)], 105, "Sheet1!E32").
dd(r59, region(West), [pl(2005, 'Budget Outgo', 'Year Total')], 390, "Sheet1!F32").
dd(r59, region(West), [pl(2005, 'Actual Income', Q1)], 160, "Sheet1!B33").
dd(r59, region(West), [pl(2005, 'Actual Income', Q2)], 155, "Sheet1!C33").
dd(r59, region(West), [pl(2005, 'Actual Income', Q3)], 90, "Sheet1!D33").
dd(r59, region(West), [pl(2005, 'Actual Income', Q4)], 177, "Sheet1!E33").
dd(r59, region(West), [pl(2005, 'Actual Income', 'Year Total')], 582, "Sheet1!F33").
dd(r59, region(West), [pl(2005, 'Actual Outgo', Q1)], 90, "Sheet1!B34").
dd(r59, region(West), [pl(2005, 'Actual Outgo', Q2)], 95, "Sheet1!C34").
dd(r59, region(West), [pl(2005, 'Actual Outgo', Q3)], 110, "Sheet1!D34").
dd(r59, region(West), [pl(2005, 'Actual Outgo', Q4)], 135, "Sheet1!E34").
dd(r59, region(West), [pl(2005, 'Actual Outgo', 'Year Total')], 430, "Sheet1!F34").
dd(r59, region(West), [pl(2005, Profit, Q1)], 70, "Sheet1!B35").
dd(r59, region(West), [pl(2005, Profit, Q2)], 60, "Sheet1!C35").
dd(r59, region(West), [pl(2005, Profit, Q3)], -20, "Sheet1!D35").
dd(r59, region(West), [pl(2005, Profit, Q4)], 42, "Sheet1!E35").
dd(r59, region(West), [pl(2005, Profit, 'Year Total')], 152, "Sheet1!F35").
dd(r59, region(East), [pl(2005, 'budget income', Q1)], 110, "Sheet1!B22").
dd(r59, region(East), [pl(2005, 'budget income', Q2)], 120, "Sheet1!C22").
dd(r59, region(East), [pl(2005, 'budget income', Q3)], 140, "Sheet1!D22").
dd(r59, region(East), [pl(2005, 'budget income', Q4)], 160, "Sheet1!E22").
dd(r59, region(East), [pl(2005, 'budget income', 'Year Total')], 530, "Sheet1!F22").
dd(r59, region(East), [pl(2005, 'Budget Outgo', Q1)], 90, "Sheet1!B23").
dd(r59, region(East), [pl(2005, 'Budget Outgo', Q2)], 95, "Sheet1!C23").
dd(r59, region(East), [pl(2005, 'Budget Outgo', Q3)], 100, "Sheet1!D23").
dd(r59, region(East), [pl(2005, 'Budget Outgo', Q4)], 105, "Sheet1!E23").
dd(r59, region(East), [pl(2005, 'Budget Outgo', 'Year Total')], 390, "Sheet1!F23").
dd(r59, region(East), [pl(2005, 'Actual Income', Q1)], 140, "Sheet1!B24").
dd(r59, region(East), [pl(2005, 'Actual Income', Q2)], 130, "Sheet1!C24").
dd(r59, region(East), [pl(2005, 'Actual Income', Q3)], 200, "Sheet1!D24").
dd(r59, region(East), [pl(2005, 'Actual Income', Q4)], 166, "Sheet1!E24").
dd(r59, region(East), [pl(2005, 'Actual Income', 'Year Total')], 636, "Sheet1!F24").
dd(r59, region(East), [pl(2005, 'Actual Outgo', Q1)], 90, "Sheet1!B25").
dd(r59, region(East), [pl(2005, 'Actual Outgo', Q2)], 95, "Sheet1!C25").
dd(r59, region(East), [pl(2005, 'Actual Outgo', Q3)], 99, "Sheet1!D25").
dd(r59, region(East), [pl(2005, 'Actual Outgo', Q4)], 135, "Sheet1!E25").
dd(r59, region(East), [pl(2005, 'Actual Outgo', 'Year Total')], 419, "Sheet1!F25").
dd(r59, region(East), [pl(2005, Profit, Q1)], 50, "Sheet1!B26").
dd(r59, region(East), [pl(2005, Profit, Q2)], 35, "Sheet1!C26").
dd(r59, region(East), [pl(2005, Profit, Q3)], 101, "Sheet1!D26").
dd(r59, region(East), [pl(2005, Profit, Q4)], 31, "Sheet1!E26").
dd(r59, region(East), [pl(2005, Profit, 'Year Total')], 217, "Sheet1!F26").
dd(r59, region(West), [pl(2004, 'budget income', Q1)], 160, "Sheet1!B13").
dd(r59, region(West), [pl(2004, 'budget income', Q2)], 120, "Sheet1!C13").
dd(r59, region(West), [pl(2004, 'budget income', Q3)], 110, "Sheet1!D13").
dd(r59, region(West), [pl(2004, 'budget income', Q4)], 120, "Sheet1!E13").
dd(r59, region(West), [pl(2004, 'budget income', 'Year Total')], 510, "Sheet1!F13").
dd(r59, region(West), [pl(2004, 'Budget Outgo', Q1)], 90, "Sheet1!B14").
dd(r59, region(West), [pl(2004, 'Budget Outgo', Q2)], 95, "Sheet1!C14").
dd(r59, region(West), [pl(2004, 'Budget Outgo', Q3)], 100, "Sheet1!D14").
dd(r59, region(West), [pl(2004, 'Budget Outgo', Q4)], 105, "Sheet1!E14").
dd(r59, region(West), [pl(2004, 'Budget Outgo', 'Year Total')], 390, "Sheet1!F14").
dd(r59, region(West), [pl(2004, 'Actual Income', Q1)], 150, "Sheet1!B15").
dd(r59, region(West), [pl(2004, 'Actual Income', Q2)], 145, "Sheet1!C15").
dd(r59, region(West), [pl(2004, 'Actual Income', Q3)], 120, "Sheet1!D15").
dd(r59, region(West), [pl(2004, 'Actual Income', Q4)], 130, "Sheet1!E15").
dd(r59, region(West), [pl(2004, 'Actual Income', 'Year Total')], 545, "Sheet1!F15").
dd(r59, region(West), [pl(2004, 'Actual Outgo', Q1)], 90, "Sheet1!B16").
dd(r59, region(West), [pl(2004, 'Actual Outgo', Q2)], 95, "Sheet1!C16").
dd(r59, region(West), [pl(2004, 'Actual Outgo', Q3)], 110, "Sheet1!D16").
dd(r59, region(West), [pl(2004, 'Actual Outgo', Q4)], 90, "Sheet1!E16").
dd(r59, region(West), [pl(2004, 'Actual Outgo', 'Year Total')], 385, "Sheet1!F16").
dd(r59, region(West), [pl(2004, Profit, Q1)], 60, "Sheet1!B17").
dd(r59, region(West), [pl(2004, Profit, Q2)], 50, "Sheet1!C17").
dd(r59, region(West), [pl(2004, Profit, Q3)], 10, "Sheet1!D17").
dd(r59, region(West), [pl(2004, Profit, Q4)], 40, "Sheet1!E17").
dd(r59, region(West), [pl(2004, Profit, 'Year Total')], 160, "Sheet1!F17").
dd(r59, region(East), [pl(2004, 'budget income', Q1)], 100, "Sheet1!B4").
dd(r59, region(East), [pl(2004, 'budget income', Q2)], 120, "Sheet1!C4").
dd(r59, region(East), [pl(2004, 'budget income', Q3)], 140, "Sheet1!D4").
dd(r59, region(East), [pl(2004, 'budget income', Q4)], 160, "Sheet1!E4").
dd(r59, region(East), [pl(2004, 'budget income', 'Year Total')], 520, "Sheet1!F4").
dd(r59, region(East), [pl(2004, 'Budget Outgo', Q1)], 90, "Sheet1!B5").
dd(r59, region(East), [pl(2004, 'Budget Outgo', Q2)], 95, "Sheet1!C5").
dd(r59, region(East), [pl(2004, 'Budget Outgo', Q3)], 100, "Sheet1!D5").
dd(r59, region(East), [pl(2004, 'Budget Outgo', Q4)], 105, "Sheet1!E5").
dd(r59, region(East), [pl(2004, 'Budget Outgo', 'Year Total')], 390, "Sheet1!F5").
dd(r59, region(East), [pl(2004, 'Actual Income', Q1)], 150, "Sheet1!B6").
dd(r59, region(East), [pl(2004, 'Actual Income', Q2)], 130, "Sheet1!C6").
dd(r59, region(East), [pl(2004, 'Actual Income', Q3)], 667, "Sheet1!D6").
dd(r59, region(East), [pl(2004, 'Actual Income', Q4)], 165, "Sheet1!E6").
dd(r59, region(East), [pl(2004, 'Actual Income', 'Year Total')], 1112, "Sheet1!F6").
dd(r59, region(East), [pl(2004, 'Actual Outgo', Q1)], 90, "Sheet1!B7").
dd(r59, region(East), [pl(2004, 'Actual Outgo', Q2)], 200, "Sheet1!C7").
dd(r59, region(East), [pl(2004, 'Actual Outgo', Q3)], 110, "Sheet1!D7").
dd(r59, region(East), [pl(2004, 'Actual Outgo', Q4)], 135, "Sheet1!E7").
dd(r59, region(East), [pl(2004, 'Actual Outgo', 'Year Total')], 535, "Sheet1!F7").
dd(r59, region(East), [pl(2004, Profit, Q1)], 60, "Sheet1!B8").
dd(r59, region(East), [pl(2004, Profit, Q2)], -70, "Sheet1!C8").
dd(r59, region(East), [pl(2004, Profit, Q3)], 557, "Sheet1!D8").
dd(r59, region(East), [pl(2004, Profit, Q4)], 30, "Sheet1!E8").
dd(r59, region(East), [pl(2004, Profit, 'Year Total')], 577, "Sheet1!F8").

r(r59, a1, "east2004 = region['East'].pl[2004, 'Profit', `Year Total`]").
r(r59, h8, 
"problem[?r,?q,?y] = 'Took a loss' WHEN
   region[?r].pl[?y,'actual income',?q] <
   region[?r].pl[?y,'actual outgo',?q]" ).

r(r59, h11,
"problem[?r,?q,?y] = 'Less than last year' WHEN
   region[?r].pl[?y,'actual income',?q] <
   region[?r].pl[?yy,'actual income',?q] AND ?yy = ?y - 1" ).

r(r59, h14,
"problem[?r,?q,?y] = 'Below budget income'  WHEN
   region[?r].pl[?y,'budget income',?q] >
   region[?r].pl[?y,'actual income',?q]" ).

r(r59, h22,
"problems = SORT( FINDALL( [?r, ?y, ?q, ?p],
   problem[?r, ?q, ?y] = ?p ) )" ).

/*
		East	2004			
						
	Q1	Q2	Q3	Q4	Year Total	
Budget Income	100	120	140	160	520	
Budget Outgo	90	95	100	105	390	
Actual Income	150	130	667	165	1112	
Actual Outgo	90	200	110	135	535	
Profit	60	-70	557	30	577	
						
		West	2004			
						
	Q1	Q2	Q3	Q4	Year Total	
Budget Income	160	120	110	120	510	
Budget Outgo	90	95	100	105	390	
Actual Income	150	145	120	130	545	
Actual Outgo	90	95	110	90	385	
Profit	60	50	10	40	160	
						
		East	2005			
						
	Q1	Q2	Q3	Q4	Year Total	
Budget Income	110	120	140	160	530	
Budget Outgo	90	95	100	105	390	
Actual Income	140	130	200	166	636	
Actual Outgo	90	95	99	135	419	
Profit	50	35	101	31	217	
						
		West	2005			
						
	Q1	Q2	Q3	Q4	Year Total	
Budget Income	100	120	140	160	520	
Budget Outgo	90	95	100	105	390	
Actual Income	160	155	90	177	582	
Actual Outgo	90	95	110	135	430	
Profit	70	60	-20	42	152	
*/

q(r59, "FIND problems",
   [[East, 2004, Q2, 'Took a loss'],
    [East, 2005, Q1, 'Less than last year'],
    [East, 2005, Q3, 'Less than last year'],
    [East, 2005, 'Year Total', 'Less than last year'],
    [West, 2004, Q1, 'Below budget income'],
    [West, 2005, Q3, 'Took a loss']] ).

q(r59, "FIND region['East'].pl[2004, 'Profit', 'Year Total']", 577).
q(r59, "FIND east2004", 577).

%-----------------------------------------------------------------------------------------
case(60, [ rulesets = [r60] ]).

r(r60, a0, "a = 2 when (3 - 2) >= 7").
r(r60, a1, "a = 2 when 7 <= (3 - 2)").
r(r60, a2, "a = 2 when (7 > 3 OR (3 - 2) >= 7) AND NOT c").
r(r60, a12, "salary = 'Ok' when (((input[`Net income`]) - (input[`Monthly expenditure`])) >= 2 * monthlypayments)").
r(r60, a14, "salary = `Not Ok` when (1/2)*(input[`Net income`] - input[`Monthly expenditure`]) < 2 * monthlypayments").

r(r60, b1, "input['Monthly expenditure'] = 100").
r(r60, b2, "input['Net income'] = 600").
r(r60, b3, "monthlypayments = 200").

q(r60, "FIND salary", ok).

%-----------------------------------------------------------------------------------------
case(61, [ rulesets = [r61] ]).

dd(r61, dd('a a'), '[]', 11, "Sheet1!E2").
dd(r61, dd('b b'), '[]', 22, "Sheet1!E3").

a(r61, (know(D, '[]', _X1, 'Sheet1!B5', [obj([D]) = _X1]) ) ).

a(r61, (rule('Sheet1!B5', [obj([D]) = _X1]) :- 
    true,
    evallist([find(r61, obj([dd("b b")]), _X2) = _X1]) ) ).

q(r61, "find d", 22).

%-----------------------------------------------------------------------------------------
case(62, [ rulesets = [r62] ]).

r(r62, a1, "adate = date(2005, 6, 1)" ).
r(r62, a2, "bdate = date(2005, 10, 1)" ).
r(r62, a3, "x = 1 WHEN (bdate - adate) < 3 months" ).
r(r62, a4, "x = 2").

q(r62, "FIND x", 2).

%-----------------------------------------------------------------------------------------
case(63, [ rulesets = [r63] ]).


r(r63, a1, "Table rate").
r(r63, a2, ["weekend", "daytime", "rate"]).
r(r63, a3, ["'yes'", "*", "0.05"]).
r(r63, a4, ["'no'", "'yes'", "0.12"]).
r(r63, a5, ["'no'", "'no'", "0.09"]).
r(r63, a6, []).
r(r63, a11, "Table weekend").
r(r63, a12, ["day", "weekend"]).
r(r63, a13, ["'saturday'", "'yes'"]).
r(r63, a14, ["'sunday'", "'yes'"]).
r(r63, a15, ["*", "'no'"]).
r(r63, a16, []).
r(r63, a21, "Table daytime").
r(r63, a22, ["time", "daytime"]).
r(r63, a23, ["> 800 and < 1800 or > 300 and < 500", "'yes'"]).
r(r63, a24, ["<= 800 or >= 1800", "'no'"]).
r(r63, a26, []).

r(r63, a27, "x when a > 20 and a < 30 or a > 3 and a < 10").


q(r63, "FIND rate WHEN day = 'monday' and time = 1200", 0.12).
q(r63, "FIND rate WHEN day = 'wednesday' and time = 800", 0.09).
q(r63, "FIND rate WHEN day = 'wednesday' and time = 400", 0.12).

q(r63, "FIND x when a = 5", true).
q(r63, "FIND x when a = 25", true).
q(r63, "FIND x when a = 15", false).


%-----------------------------------------------------------------
case(64, [ rulesets = [r64] ]).

d(r64, loans, [
  [1,2,3,4],
  ['LOAN AMT', FICO, LTV, 'DOC TYPE'],
  [222000, 700, 90, partial],
  [111000, 750, 85, 'FULL DOC'],
  [333000, 100, 100, none],
  [244000, 720, 93, none] ] ).

r(r64, a1, "ok[?i] = 'yes' WHEN large[?i] = 'yes' AND loans[?i, 'FICO'] >= 700 AND loans[?i, 'LTV'] >= 90 ").
r(r64, a2, "ok[?i] = 'yes' WHEN large[?i] = 'no' AND loans[?i, 'FICO'] >= 720 AND loans[?i, 'DOC TYPE'] = 'FULL DOC' ").
r(r64, a3, "ok[?i] = 'no'").
r(r64, a4, "large[?i] = 'yes' WHEN loans[?i, 'LOAN AMT'] >= 200000").
r(r64, a5, "large[?i] = 'no' WHEN loans[?i, 'LOAN AMT'] < 200000").
r(r64, a6, "oks = FINDALL( ?i, ok[?i] = 'yes')").

q(r64, "FIND ok[1]", yes).
q(r64, "FIND ok[2]", yes).
q(r64, "FIND oks", [1, 4, 2]).


%-----------------------------------------------------------------
case(65, [ rulesets = [r65] ]).

d(r65, loans, [
  [1,2,3,4],
  [AMT, FICO, LTV],
  [1300, 30, 3],
  [900, 2, 20],
  [1400, 4, 4],
  [1400, 40, 4] ] ).

r(r65, a1, "ok[?i] = 'yes' WHEN large[?i] = 'yes' AND loans[?i, 'FICO'] > 10").
r(r65, a2, "ok[?i] = 'yes' WHEN large[?i] <> 'yes' AND loans[?i, 'LTV'] > 10").
r(r65, a3, "ok[?i] = 'no'").
r(r65, a4, "large[?i] = 'yes' WHEN loans[?i, 'AMT'] > 1000").
r(r65, a5, "large[?i] = 'no' WHEN loans[?i, 'AMT'] <= 1000").
r(r65, a7, "rows = FINDALL( ?i, ?i = INDEX(loans, 1) )").
r(r65, a8, "cols = FINDALL( ?i, ?i = INDEX(loans, 2) )").
r(r65, a9, "oks = COLLECT ?i FOREACH ?i = INDEX(loans, 1) WHERE ok[?i] = 'yes'").
r(r65, a9, "yns['yes'] = COLLECT ?i FOREACH ?i = INDEX(loans, 1) WHERE ok[?i] = 'yes' WHEN ?y = 'yes'").
r(r65, a9, "yns['no'] = COLLECT ?i FOREACH ?i = INDEX(loans, 1) WHERE ok[?i] = 'no' WHEN ?y = 'no'").
r(r65, a12, "ad['this'] = 4").
r(r65, a13, "ad['that'] = 7").
r(r65, a14, "ads = COLLECT( [?i, ?x], ?i = INDEX(ad, 1), ?x = ad[?i])").
r(r65, a15, "sum1 = SUM( [ad['this'], ad['that'] ] )").
r(r65, a16, "sum2 = SUM( COLLECT( ?x, ?i = INDEX(ad, 1), ?x = ad[?i] ) )").

q(r65, "FIND ok[1]", yes).
q(r65, "FIND ok[2]", yes).
q(r65, "FIND rows", [1,2,3,4]).
q(r65, "FIND cols", [AMT, FICO, LTV]).
q(r65, "FIND oks", [1, 2, 4]).
q(r65, "FIND yns['yes']", [1, 2, 4]).
q(r65, "FIND yns['no']", [3]).
q(r65, "FIND ads", [[this,4],[that,7]]).
q(r65, "FIND sum1", 11).
q(r65, "FIND sum2", 11).

%-----------------------------------------------------------------------------------------
case(66, [ rulesets = [r66] ]).


r(r66, a1, "Table invest[?inv,?i]").
r(r66, a2, ["?inv", "invest[?inv,?i]"]).
r(r66, a3, ["'GMAC'", "'yes'"]).
r(r66, a4, ["'Citi'", "'no'"]).
r(r66, a6, []).

q(r66, "FIND invest['GMAC', 3]", yes).
q(r66, "FIND invest['Citi', 3]", no).

%-----------------------------------------------------------------------------------------
case(67, [ rulesets = [r67] ]).

r(r67, a1, "startdate = date(2002,2,2)").
r(r67, a2, "visit[?x] = startdate + ?y days WHEN ?y = 14 * ?x").
r(r67, a3, "zz[?x] = 3 * ?x").
r(r67, a4, "enddate = date(2002,4,2)").
r(r67, a5, "days = DAYS_BETWEEN(startdate, enddate)").

q(r67, "FIND visit[2]", "2002-3-2").
q(r67, "FIND zz[2]", 6).
q(r67, "FIND days", 59).

%-----------------------------------------------------------------------------------------
case(68, [ rulesets = [r68] ]).

r(r68, a1, "Q = '?x WHEN ( ?x = MEMBER( [1,2,3] ) AND NOT ?x = MEMBER( [2,4,6] ) )' ").
r(r68, a2, "a = FINDALL(?x, (?x = MEMBER( [1,2,3] )))").
r(r68, a3, "b = DYNAMIC_FIND(Q)").
r(r68, a4, "P = '?x * ?y WHEN ( ?x = MEMBER( [1,2] ) AND ?y = MEMBER( [1,2] ) )' ").
r(r68, a5, "c = DYNAMIC_FIND(P)").

q(r68, "FIND a", [1,2,3]).
q(r68, "FIND b", [1,3]).
q(r68, "FIND c", [1*1, 1*2, 2*1, 2*2]).

%-----------------------------------------------------------------------------------------
case(69, [ rulesets = [r69] ]).

r(r69, a1, "n = NOW()").
r(r69, a1, "t = TODAY()").
r(r69, a3, "ir = IS_RULESET('r69')").

q(r69, "FIND n", ?s) :- datetime_get(now, ?x), format_answer(?x, ?s).
q(r69, "FIND t", ?s) :- datetime_get(today, ?x), format_answer(?x, ?s).
q(r69, "FIND ir", true).

%----------------------------------------------------------------------
case(70, [ rulesets = [r70] ]).

d(r70, Meals, [
  [fruit, meat],
  [breakfast, lunch, dinner],
  [banana, apple, grape],
  [sausage, ham, steak] ] ).

r(r70, a1, "food_out = Meals[ course_in, meal_in ]").
r(r70, a2, "meal_out = ?meal AND course_out = ?course WHEN Meals[?course, ?meal] = food_in").
r(r70, a3, "meal_in = 'lunch'").
r(r70, a4, "course_in = 'meat'").
r(r70, a5, "food_in = 'banana'").

q(r70, "FIND food_out", ham).
q(r70, "FIND meal_out AND course_out", [breakfast, fruit]).

%----------------------------------------------------------------------
case(71, [ rulesets = [r71] ]).

r(r71, a1, "birthdate1 = date(2009, 1, 15)").
r(r71, a1, "birthdate2 = date(2009, 4, 15)").
r(r71, a3, "todaydate = date(2009, 7, 15)").
r(r71, a4, "age1 = todaydate - birthdate1").
r(r71, a5, "older1 = 'true' when age1 >= 24 weeks").
r(r71, a6, "older1 = 'false' when age1 < 24 weeks").
r(r71, a7, "age2 = todaydate - birthdate2").
r(r71, a8, "older2 = 'true' when age2 >= 24 weeks").
r(r71, a9, "older2 = 'false' when age2 < 24 weeks").

q(r71, "FIND age1", "6 months").
q(r71, "FIND older1", 8).
q(r71, "FIND age2", "6 months").
q(r71, "FIND older2", 8).

%------------------------------------------------------------------------
case(72, [ rulesets = [r72] ]).

r(r72, a1, "start = datetime(2009, 1, 15, 15, 0, 0)").
r(r72, a2, "end = datetime(2009, 1, 15, 19, 0, 0)").
r(r72, a3, "mins = MINUTES_BETWEEN(endtime, starttime)").
r(r72, a4, "starttime = EXTRACT_TIME(start)").
r(r72, a5, "endtime = EXTRACT_TIME(end)").
r(r72, a6, "startdate = EXTRACT_DATE(start)").
r(r72, a7, "enddate = EXTRACT_DATE(end)").
r(r72, a8, "evening_start = 'true' when starttime >= time(18,0,0)").
r(r72, a9, "evening_end = 'true' when endtime >= time(18,0,0)").
r(r72, a10, "day_minutes = time(18,0,0) - starttime").
r(r72, a11, "days = startdate - date(2009, 1, 0)").


q(r72, "FIND mins", 240).
q(r72, "FIND starttime", "15:0:0").
q(r72, "FIND startdate", "2009-1-15").
q(r72, "FIND evening_start", 'false').
q(r72, "FIND evening_end", 'true').
q(r72, "FIND day_minutes", 3 hours).
q(r72, "FIND days", 15 days).





