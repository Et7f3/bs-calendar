type tm = {
  tm_sec: int,
  tm_min: int,
  tm_hour: int,
  tm_mday: int,
  tm_mon: int,
  tm_year: int,
  tm_wday: int,
  tm_yday: int,
  tm_isdst: bool,
};
[@bs.scope "Date"] [@bs.val] external timeMs: unit => float = "now";
/* FIXME: write better in upstream. */
let gettimeofday = () => timeMs() /. 1000.;

let bug = [%bs.raw "x => x | 0"]; /* FIXME: this is int_of_float that emit a warning */
let time = () => bug(gettimeofday());

let leap = y => y mod 4 == 0 && y mod 100 != 0 || y mod 400 == 0;

let yday = (dd, mm, aaaa) => {
  let dperm = [|31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31|];
  let rec yday' = mm =>
    if (mm == 2 && leap(aaaa)) {
      dperm[mm] + 1 + yday'(mm - 1);
    } else if (mm >= 0) {
      dperm[mm] + yday'(mm - 1);
    } else {
      dd;
    };
  yday'(mm - 1);
};

let localtime = f => {
  open Js.Date;
  let t = fromFloat(f *. 1000.);
  let dd = int_of_float(getDate(t));
  let mm = int_of_float(getMonth(t));
  let aaaa = int_of_float(getFullYear(t));
  {
    tm_sec: int_of_float(getSeconds(t)),
    tm_min: int_of_float(getMinutes(t)),
    tm_hour: int_of_float(getHours(t)),
    tm_mday: dd,
    tm_mon: mm,
    tm_year: aaaa - 1900,
    tm_wday: int_of_float(getDay(t)),
    tm_yday: yday(dd, mm, aaaa),
    tm_isdst: false,
  };
};

let gmtime = f => {
  open Js.Date;
  let t = fromFloat(f *. 1000.);
  let dd = int_of_float(getUTCDate(t));
  let mm = int_of_float(getUTCMonth(t));
  let aaaa = int_of_float(getUTCFullYear(t));
  {
    tm_sec: int_of_float(getUTCSeconds(t)),
    tm_min: int_of_float(getUTCMinutes(t)),
    tm_hour: int_of_float(getUTCHours(t)),
    tm_mday: dd,
    tm_mon: mm,
    tm_year: aaaa - 1900,
    tm_wday: int_of_float(getUTCDay(t)),
    tm_yday: yday(dd, mm, aaaa),
    tm_isdst: false,
  };
};

if (true) {
  Js.log(Js.Date.fromFloat(0.));
  Js.log(time());
  Js.log(
    localtime(
      Js.Date.getTime(Js.Date.fromString("2024-12-31 00:00:00.000Z")) /. 1000.,
    ),
  );
  Js.log(localtime(0.));
  Js.log(gettimeofday());
  Js.log(localtime(gettimeofday()));
};
