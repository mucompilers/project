/*
 * wumpus.c --- a faithful translation of the classic "Hunt The Wumpus" game.
 *
 * Translator: Eric S. Raymond <esr@snark.thyrsus.com>
 * Version: $Id: wumpus.c,v 1.4 1996/05/17 17:30:35 esr Exp $
 *
 * This was the state of the art 20 years ago, in 1972.  We've come a long
 * way, baby.
 *
 * The BASIC source is that posted by Magnus Olsson in USENET article
 * <9207071854.AA21847@thep.lu.se>: he wrote
 *
 * >Below is the source code for _one_ (rather simple) Wumpus version,
 * >which I found in the PC-BLUE collection on SIMTEL20. I believe this is
 * >pretty much the same version that was published in David Ahl's "101
 * >Basic Computer Games" (or, possibly, in the sequel). 
 *
 * I have staunchly resisted the temptation to "improve" this game.  It
 * is functionally identical to the BASIC version (source for which
 * appears in the comments).  I fixed some typos in the help text.
 *
 * Language hackers may be interested to know that he most difficult thing
 * about the translation was tracking the details required to translate from
 * 1-origin to 0-origin array indexing.
 *
 * The only enhancement is a an -s command-line switch for setting the
 * random number seed.
 *
 * So, pretend for a little while that your workstation is an ASR-33 and
 * limber up your fingers for a trip to nostalgia-land...
 */

/* 5 REM *** HUNT THE WUMPUS ***                            */

/* 10 DIM P(5)                                              */

/* 80 REM *** SET UP CAVE (DODECAHEDRAL NODE LIST) ***            */
/* 85 DIM S(20,3)                                     */
/* 90 FOR J=1 TO 20                                         */
/* 95 FOR K=1 TO 3                                          */
/* 100 READ S(J,K)                                          */
/* 105 NEXT K                                         */
/* 110 NEXT J                                         */
/* 115 DATA 2,5,8,1,3,10,2,4,12,3,5,14,1,4,6                */
/* 120 DATA 5,7,15,6,8,17,1,7,9,8,10,18,2,9,11              */
/* 125 DATA 10,12,19,3,11,13,12,14,20,4,13,15,6,14,16       */
/* 130 DATA 15,17,20,7,16,18,9,17,19,11,18,20,13,16,19            */
fn newWorld() -> World {
      return World {
            inputs :
                  [ b'Y', b'M', b'1', b'M', b'8', b'M', b'9', b'M', b'1', b'8'
                  , b'M', b'1', b'9', b'M', b'4', b'M', b'1', b'4', b'M', b'1'
                  , b'3', b'M', b'1', b'2', b'M', b'1', b'1', b'S', b'1', b'.'
                  , b'1', b'2', b'.'
                  ],
            nextip : 0,
            cave : [
                  [1,4,7],
                  [0,2,9],
                  [1,3,11],
                  [2,4,13],
                  [0,3,5],
                  [4,6,14],
                  [5,7,16],
                  [0,6,8],
                  [7,9,17],
                  [1,8,10],
                  [9,11,18],
                  [2,10,12],
                  [11,13,19],
                  [3,12,14],
                  [5,13,15],
                  [14,16,19],
                  [6,15,17],
                  [8,16,18],
                  [10,17,19],
                  [12,15,18]
            ],
         rand : 5,
         finished : 0,
         save : [0, 0, 0, 0, 0, 0],
         arrows : 5,
         scratchloc : 0,

         you : 1,
         wumpus : 10,
         pit1 : 5,
         pit2 : 9,
         bats1 : 14,
         bats2 : 18
      };
}

struct World {
      inputs : [u8;33],
      nextip : i32,
      cave : [[i32;3];20],
      rand : i32,
      finished : i32,
      save : [i32;6],
      arrows : i32,
      scratchloc : i32,

      you : i32,
      wumpus : i32,
      pit1 : i32,
      pit2 : i32,
      bats1 : i32,
      bats2 : i32
}

struct WorldNum {
      w : World,
      n : i32
}

struct WorldLet {
      w : World,
      l : u8
}

struct WorldBool {
      w : World,
      b : bool
}

/* 135 DEF FNA(X)=INT(20*RND(1))+1                          */
fn FNA(mut w : World) -> WorldNum {
      w.rand += 79;
      let r = w.rand % 20;
      return WorldNum { w : w, n : r };
}

/* 140 DEF FNB(X)=INT(3*RND(1))+1                           */
fn FNB(mut w : World) -> WorldNum {
      w.rand += 79;
      let r = w.rand % 3;
      return WorldNum { w : w, n : r };
}

/* 145 DEF FNC(X)=INT(4*RND(1))+1                           */
fn FNC(mut w : World) -> WorldNum {
      w.rand += 89;
      let r = w.rand % 4;
      return WorldNum { w : w, n : r };
}

fn getnum(prompt : &[u8], mut w : World) -> WorldNum {
      prints(prompt);
      prints(b"?");
      let mut n = w.nextip;
      w.nextip += 1;
      let ip1 = w.inputs[n];
      n = w.nextip;
      w.nextip += 1;
      let ip2 = w.inputs[n];

      if (isnum(ip2)) {
            return WorldNum { w : w, n : atoi(ip1, ip2) };
      };
      w.nextip -= 1;
      return WorldNum { w : w, n : atoi(b'0', ip1) };
}

fn getlet(prompt : &[u8], mut w : World) -> WorldLet {
      prints(prompt);
      prints(b"?");
      let mut n = w.nextip;
      w.nextip += 1;
      let mut ip = w.inputs[n];
      while (ip == b'.') {
            n = w.nextip;
            w.nextip += 1;
            ip = w.inputs[n];
      };

      return WorldLet { w : w, l : ip };
}

fn isnum(c : u8) -> bool {
      return if (c == b'0' || c == b'1' || c == b'2' || c == b'3' || c == b'4'
            || c == b'5' || c == b'6' || c == b'7' || c == b'8' || c == b'9')
            { true } else { false };
}

fn atoi_digit(c : u8) -> i32 {
      if (c == b'0') {
            return 0;
      };
      if (c == b'1') {
            return 1;
      };
      if (c == b'2') {
            return 2;
      };
      if (c == b'3') {
            return 3;
      };
      if (c == b'4') {
            return 4;
      };
      if (c == b'5') {
            return 5;
      };
      if (c == b'6') {
            return 6;
      };
      if (c == b'7') {
            return 7;
      };
      if (c == b'8') {
            return 8;
      };
      if (c == b'9') {
            return 9;
      };
      return -1;
}

fn atoi(c1 : u8, c2 : u8) -> i32 {
      return 10 * atoi_digit(c1) + atoi_digit(c2);
}

fn print_instructions() {
      /* 375 REM *** INSTRUCTIONS ***                                   */
      /* 380 PRINT "WELCOME TO 'HUNT THE WUMPUS'"                       */
      prints(b"WELCOME TO 'HUNT THE WUMPUS'");
      /* 385 PRINT "  THE WUMPUS LIVES IN A CAVE OF 20 ROOMS. EACH ROOM"      */
      prints(b"  THE WUMPUS LIVES IN A CAVE OF 20 ROOMS. EACH ROOM");
      /* 390 PRINT "HAS 3 TUNNELS LEADING TO OTHER ROOMS. (LOOK AT A"         */
      prints(b"HAS 3 TUNNELS LEADING TO OTHER ROOMS. (LOOK AT A");
      /* 395 PRINT "DODECAHEDRON TO SEE HOW THIS WORKS-IF YOU DON'T KNOW"     */
      prints(b"DODECAHEDRON TO SEE HOW THIS WORKS-IF YOU DON'T KNOW");
      /* 400 PRINT "WHAT A DODECAHEDRON IS, ASK SOMEONE)"               */
      prints(b"WHAT A DODECAHEDRON IS, ASK SOMEONE)");
      /* 405 PRINT                                                */
      prints(b"");
      /* 410 PRINT "     HAZARDS:"                                */
      prints(b"     HAZARDS:");
      /* 415 PRINT " BOTTOMLESS PITS - TWO ROOMS HAVE BOTTOMLESS PITS IN THEM */
      prints(b" BOTTOMLESS PITS - TWO ROOMS HAVE BOTTOMLESS PITS IN THEM");
      /* 420 PRINT "     IF YOU GO THERE, YOU FALL INTO THE PIT (& LOSE!)"    */
      prints(b"     IF YOU GO THERE, YOU FALL INTO THE PIT (& LOSE!)");
      /* 425 PRINT " SUPER BATS - TWO OTHER ROOMS HAVE SUPER BATS. IF YOU"    */
      prints(b" SUPER BATS - TWO OTHER ROOMS HAVE SUPER BATS. IF YOU");
      /* 430 PRINT "     GO THERE, A BAT GRABS YOU AND TAKES YOU TO SOME OTHER"     */
      prints(b"     GO THERE, A BAT GRABS YOU AND TAKES YOU TO SOME OTHER");
      /* 435 PRINT "     ROOM AT RANDOM. (WHICH MAY BE TROUBLESOME)"          */
      prints(b"     ROOM AT RANDOM. (WHICH MAY BE TROUBLESOME)");
      /* 440 INPUT "TYPE AN E THEN RETURN ";W9                    */
      // getlet("TYPE AN E THEN RETURN ");
      /* 445 PRINT "     WUMPUS:"                                 */
      prints(b"     WUMPUS:");
      /* 450 PRINT " THE WUMPUS IS NOT BOTHERED BY HAZARDS (HE HAS SUCKER"    */
      prints(b" THE WUMPUS IS NOT BOTHERED BY HAZARDS (HE HAS SUCKER");
      /* 455 PRINT " FEET AND IS TOO BIG FOR A BAT TO LIFT).  USUALLY"  */
      prints(b" FEET AND IS TOO BIG FOR A BAT TO LIFT).  USUALLY");
      /* 460 PRINT " HE IS ASLEEP.  TWO THINGS WAKE HIM UP: YOU SHOOTING AN"  */
      prints(b" HE IS ASLEEP.  TWO THINGS WAKE HIM UP: YOU SHOOTING AN");
      /* 465 PRINT "ARROW OR YOU ENTERING HIS ROOM."                    */
      prints(b"ARROW OR YOU ENTERING HIS ROOM.");
      /* 470 PRINT "     IF THE WUMPUS WAKES HE MOVES (P=.75) ONE ROOM" */
      prints(b"     IF THE WUMPUS WAKES HE MOVES (P=.75) ONE ROOM");
      /* 475 PRINT " OR STAYS STILL (P=.25).  AFTER THAT, IF HE IS WHERE YOU" */
      prints(b" OR STAYS STILL (P=.25).  AFTER THAT, IF HE IS WHERE YOU");
      /* 480 PRINT " ARE, HE EATS YOU UP AND YOU LOSE!"                 */
      prints(b" ARE, HE EATS YOU UP AND YOU LOSE!");
      /* 485 PRINT                                                */
      prints(b"");
      /* 490 PRINT "     YOU:"                                    */
      prints(b"     YOU:");
      /* 495 PRINT " EACH TURN YOU MAY MOVE OR SHOOT A CROOKED ARROW"         */
      prints(b" EACH TURN YOU MAY MOVE OR SHOOT A CROOKED ARROW");
      /* 500 PRINT "   MOVING:  YOU CAN MOVE ONE ROOM (THRU ONE TUNNEL)"      */
      prints(b"   MOVING:  YOU CAN MOVE ONE ROOM (THRU ONE TUNNEL)");
      /* 505 PRINT "   ARROWS:  YOU HAVE 5 ARROWS.  YOU LOSE WHEN YOU RUN OUT */
      prints(b"   ARROWS:  YOU HAVE 5 ARROWS.  YOU LOSE WHEN YOU RUN OUT");
      /* 510 PRINT "   EACH ARROW CAN GO FROM 1 TO 5 ROOMS. YOU AIM BY TELLING*/
      prints(b"   EACH ARROW CAN GO FROM 1 TO 5 ROOMS. YOU AIM BY TELLING");
      /* 515 PRINT "   THE COMPUTER THE ROOM#S YOU WANT THE ARROW TO GO TO."  */
      prints(b"   THE COMPUTER THE ROOM#S YOU WANT THE ARROW TO GO TO.");
      /* 520 PRINT "   IF THE ARROW CAN'T GO THAT WAY (IF NO TUNNEL) IT MOVES"*/
      prints(b"   IF THE ARROW CAN'T GO THAT WAY (IF NO TUNNEL) IT MOVES");
      /* 525 PRINT "   AT RANDOM TO THE NEXT ROOM."                     */
      prints(b"   AT RANDOM TO THE NEXT ROOM.");
      /* 530 PRINT "     IF THE ARROW HITS THE WUMPUS, YOU WIN."        */
      prints(b"     IF THE ARROW HITS THE WUMPUS, YOU WIN.");
      /* 535 PRINT "     IF THE ARROW HITS YOU, YOU LOSE."              */
      prints(b"     IF THE ARROW HITS YOU, YOU LOSE.");
      /* 540 INPUT "TYPE AN E THEN RETURN ";W9                    */
      //getlet("TYPE AN E THEN RETURN ");
      /* 545 PRINT "    WARNINGS:"                                */
      prints(b"    WARNINGS:");
      /* 550 PRINT "     WHEN YOU ARE ONE ROOM AWAY FROM A WUMPUS OR HAZARD," */
      prints(b"     WHEN YOU ARE ONE ROOM AWAY FROM A WUMPUS OR HAZARD,");
      /* 555 PRINT "     THE COMPUTER SAYS:"                            */
      prints(b"     THE COMPUTER SAYS:");
      /* 560 PRINT " WUMPUS:  'I SMELL A WUMPUS'"                       */
      prints(b" WUMPUS:  'I SMELL A WUMPUS'");
      /* 565 PRINT " BAT   :  'BATS NEARBY'"                            */
      prints(b" BAT   :  'BATS NEARBY'");
      /* 570 PRINT " PIT   :  'I FEEL A DRAFT'"                   */
      prints(b" PIT   :  'I FEEL A DRAFT'");
      /* 575 PRINT                                                */
      prints(b"");
      /* 580 RETURN                                               */
}

fn check_hazards(w : World) -> World {
      /* 585 REM *** PRINT LOCATION & HAZARD WARNINGS ***                 */
      /* 590 PRINT                                      */
      prints(b"");

      /* 595 FOR J=2 TO 6                                     */
      /* 600 FOR K=1 TO 3                                     */
      /* 605 IF S(L(1),K)<>L(J) THEN 640                            */
      /* 610 ON J-1 GOTO 615,625,625,635,635                        */
      /* 615 PRINT "I SMELL A WUMPUS!"                              */
      /* 620 GOTO 640                                         */
      /* 625 PRINT "I FEEL A DRAFT"                           */
      /* 630 GOTO 640                                         */
      /* 635 PRINT "BATS NEARBY!"                                   */
      /* 640 NEXT K                                     */
      /* 645 NEXT J                                     */
      let mut k = 0;
      while (k < 3) {
            let room = w.cave[w.you][k];

            if (room == w.wumpus) {
                  prints(b"I SMELL A WUMPUS!");
            } else { if (room == w.pit1 || room == w.pit2) {
                  prints(b"I FEEL A DRAFT");
            } else { if (room == w.bats1 || room == w.bats2) {
                  prints(b"BATS NEARBY!");
            }; }; };
            k += 1;
      };

      /* 650 PRINT "YOU ARE IN ROOM "L(1)                           */
      prints(b"YOU ARE IN ROOM");
      printi(w.you+1);

      /* 655 PRINT "TUNNELS LEAD TO "S(L,1);S(L,2);S(L,3)                 */
      prints(b"TUNNELS LEAD TO");
      printi(w.cave[w.you][0]+1);
      printi(w.cave[w.you][1]+1);
      printi(w.cave[w.you][2]+1);

      /* 660 PRINT                                      */
      prints(b"");

      return w;

      /* 665 RETURN                                     */
}

fn move_or_shoot(w : World) -> WorldBool {
      /* 670 REM *** CHOOSE OPTION ***                              */

      /* 675 PRINT "SHOOT OR MOVE (S-M)";                           */
      /* 680 INPUT I$                                         */
      let c = getlet(b"SHOOT OR MOVE (S-M)", w);

      /* 685 IF I$<>"S" THEN 700                                    */
      /* 690 O=1                                              */
      /* 695 RETURN                                     */
      /* 700 IF I$<>"M" THEN 675                                    */
      /* 705 O=2                                              */
      /* 710 RETURN                                     */
      if (c.l == b'S') {
            return WorldBool { w : c.w, b : true };
      } else {
            return WorldBool { w : c.w, b : false };
      };
}



fn shoot(mut w : World) -> World {
      /* 715 REM *** ARROW ROUTINE ***                              */
      /* 720 F=0                                              */
      w.finished = 0;

      /* 725 REM *** PATH OF ARROW ***                              */
      /* 735 PRINT "NO. OF ROOMS (1-5)";                            */
      /* 740 INPUT J9                                         */
      let mut wn = getnum(b"NO. OF ROOMS (1-5)", w);
      w = wn.w;
      let mut j9 = wn.n;

      while (j9 < 1 || j9 > 5) {
            wn = getnum(b"NO. OF ROOMS (1-5)", w);
            w = wn.w;
            j9 = wn.n;
      };

      /* 745 IF J9<1 THEN 735                                 */
      /* 750 IF J9>5 THEN 735                                 */

      /* 755 FOR K=1 TO J9                                    */
      let mut path = [0,0,0,0,0];
      let mut k = 0;
      while (k < j9) {
            /* 760 PRINT "ROOM #";                                */
            /* 765 INPUT P(K)                               */
            let p = getnum(b"ROOM #", w);
            w = p.w;
            path[k] = p.n - 1;
            /* 770 IF K<=2 THEN 790                               */
            /* 775 IF P(K)<>P(K-2) THEN 790                             */
            if (k <= 1 || path[k] != path[(k - 2)]) {
            } else {
                  /* 780 PRINT "ARROWS AREN'T THAT CROOKED - TRY ANOTHER ROOM"      */
                  prints(b"ARROWS AREN'T THAT CROOKED - TRY ANOTHER ROOM");
                  /* 785 GOTO 760                                       */
                  k -= 1;
            };

            /* 790 NEXT K                                         */
            k += 1;
      };

      /* 795 REM *** SHOOT ARROW ***                          */
      /* 800 L=L(1)                                     */
      let mut scratchloc = w.you;

      /* 805 FOR K=1 TO J9                                    */
      k = 0;
      while (k < j9) {
            /* 810 FOR K1=1 TO 3                                  */
            let mut k1 = 0;
            while (k1 < 3) {
                  /* 815 IF S(L,K1)=P(K) THEN 895                   */
                  if (w.cave[w.scratchloc][k1] == path[k]) {
                        /*
                         * This is the only bit of the translation I'm not sure
                         * about.  It requires the trajectory of the arrow to
                         * be a path.  Without it, all rooms on the trajectory
                         * would be required by the above to be adjacent to the
                         * player, making for a trivial game --- just move to where
                         * you smell a wumpus and shoot into all adjacent passages!
                         * However, I can't find an equivalent in the BASIC.
                         */
                        w.scratchloc = path[k];

                        /* this simulates logic at 895 in the BASIC code */
                        w = check_shot(w);
                        if (w.finished != 0) { return w; };
                  };

                  /* 820 NEXT K1                                    */
                  k1 += 1;
            };

            /* 825 REM *** NO TUNNEL FOR ARROW ***                      */
            /* 830 L=S(L,FNB(1))                                  */
            let r = FNB(w);
            w = r.w;
            w.scratchloc = w.cave[scratchloc][r.n];

            /* 835 GOTO 900                                       */
            w = check_shot(w);

            /* 840 NEXT K                                         */
            k += 1;
      };

      if (w.finished == 0) {
            /* 845 PRINT "MISSED"                                 */
            prints(b"MISSED");

            /* 850 L=L(1)                                         */
            w.scratchloc = w.you;

            /* 855 REM *** MOVE WUMPUS ***                              */
            /* 860 GOSUB 935                                */
            w = move_wumpus(w);

            /* 865 REM *** AMMO CHECK ***                         */
            /* 870 A=A-1                                          */
            /* 875 IF A>0 THEN 885                                */
            /* 880 F=-1                                     */
            w.arrows -= 1;
            if (w.arrows <= 0) { w.finished = -1; };
      };

      return w;

      /* 885 RETURN                                     */
}

fn check_shot(mut w : World) -> World {
      /* 890 REM *** SEE IF ARROW IS AT L(1) OR AT L(2)             */
      /* 895 L=P(K)                                     */

      /* 900 IF L<>L(2) THEN 920                                    */
      /* 905 PRINT "AHA! YOU GOT THE WUMPUS!"                       */
      /* 910 F=1                                              */
      /* 915 RETURN                                     */
      if (w.scratchloc == w.wumpus) {
            prints(b"AHA! YOU GOT THE WUMPUS!");
            w.finished = 1;
      }

      /* 920 IF L<>L(1) THEN 840                                    */
      /* 925 PRINT "OUCH! ARROW GOT YOU!"                           */
      /* 930 GOTO 880                                         */
      else { if (w.scratchloc == w.you) {
            prints(b"OUCH! ARROW GOT YOU!");
            w.finished = -1;
      }; };

      return w;
}

fn move_wumpus(mut w : World) -> World {
      /* 935 REM *** MOVE WUMPUS ROUTINE ***                        */
      /* 940 K=FNC(0)                                         */
      let r = FNC(w);
      w = r.w;

      /* 945 IF K=4 THEN 955                                  */
      /* 950 L(2)=S(L(2),K)                                   */
      if (r.n < 3) { w.wumpus = w.cave[w.wumpus][r.n]; };

      /* 955 IF L(2)<>L THEN 970                                    */
      if (w.wumpus != w.you) { return w; };

      /* 960 PRINT "TSK TSK TSK - WUMPUS GOT YOU!"                  */
      prints(b"TSK TSK TSK - WUMPUS GOT YOU!");

      /* 965 F=-1                                             */
      w.finished = -1;

      /* 970 RETURN                                     */
      return w;
}

fn move_you(mut w : World) -> World {
      /* 975 REM *** MOVE ROUTINE ***                         */
      /* 980 F=0                                              */
      w.finished = 0;

      /* 985 PRINT "WHERE TO";                                */
      /* 990 INPUT L                                          */
      let r = getnum(b"WHERE TO", w);
      w = r.w;
      w.scratchloc = r.n;
      w.scratchloc -= 1;

      /* 995 IF L<1 THEN 985                                  */
      /* 1000 IF L>20 THEN 985                                */
      while (w.scratchloc != w.cave[w.you][0]
                  && w.scratchloc != w.cave[w.you][1]
                  && w.scratchloc != w.cave[w.you][2]) {

            /* 1025 IF L=L(1) THEN 1045                                   */
            if (w.scratchloc != w.you) {
                  /* 1030 PRINT "NOT POSSIBLE -";                             */
                  prints(b"NOT POSSIBLE -");
            };

            let ip = getnum(b"WHERE TO", w);
            w = ip.w;
            w.scratchloc = ip.n;
            w.scratchloc -= 1;
      };

      /* 1040 REM *** CHECK FOR HAZARDS ***                   */
      /* 1045 L(1)=L                                          */
      w.you = w.scratchloc;

      while (true) {
            if (w.you == w.wumpus) {
                  /* 1050 REM *** WUMPUS ***                            */
                  /* 1055 IF L<>L(2) THEN 1090                          */
                  /* 1060 PRINT "... OOPS! BUMPED A WUMPUS!"                  */
                  /* 1065 REM *** MOVE WUMPUS ***                             */
                  /* 1070 GOSUB 940                               */
                  /* 1075 IF F=0 THEN 1090                              */
                  /* 1080 RETURN                                        */
                  prints(b"... OOPS! BUMPED A WUMPUS!");
                  w = move_wumpus(w);
                  return w;
            } else { if (w.you == w.pit1 || w.you == w.pit2) {
                  /* 1085 REM *** PIT ***                               */
                  /* 1090 IF L=L(3) THEN 1100                           */
                  /* 1095 IF L<>L(4) THEN 1120                          */
                  /* 1100 PRINT "YYYYIIIIEEEE . . . FELL IN PIT"              */
                  /* 1105 F=-1                                          */
                  /* 1110 RETURN                                        */
                  prints(b"YYYYIIIIEEEE . . . FELL IN PIT");
                  w.finished = -1;
                  return w;
            } else { if (w.you == w.bats1 || w.you == w.bats2) {
                  /* 1115 REM *** BATS ***                              */
                  /* 1120 IF L=L(5) THEN 1130                           */
                  /* 1125 IF L<>L(6) THEN 1145                          */
                  /* 1130 PRINT "ZAP--SUPER BAT SNATCH! ELSEWHEREVILLE FOR YOU!"    */
                  /* 1135 L=FNA(1)                                */
                  /* 1140 GOTO 1045                               */
                  /* 1145 RETURN                                        */
                  /* 1150 END                                     */
                  prints(b"ZAP--SUPER BAT SNATCH! ELSEWHEREVILLE FOR YOU!");
                  let l = FNA(w);
                  w = l.w;
                  w.scratchloc = l.n;
                  w.you = l.n;
            } else { return w; }; }; };
      };
      return w;
}

fn main() {
      let mut w = newWorld();

      /* 15 PRINT "INSTRUCTIONS (Y-N)";                             */
      /* 20 INPUT I$                                          */
      let c = getlet(b"INSTRUCTIONS (Y-N)", w);
      w = c.w;

      /* 25 IF I$="N" THEN 35                                 */
      /* 30 GOSUB 375                                         */
      /* 35 GOTO 80                                     */
      if (c.l == b'Y') { print_instructions(); };

      /* 150 REM *** LOCATE L ARRAY ITEMS ***                       */
      /* 155 REM *** 1-YOU, 2-WUMPUS, 3&4-PITS, 5&6-BATS ***        */
      /* 160 DIM L(6)                                         */
      /* 165 DIM M(6)                                         */
      /* 170 FOR J=1 TO 6                                     */
      /* 175 L(J)=FNA(0)                                      */
      /* 180 M(J)=L(J)                                        */
      /* 185 NEXT J                                     */
      /* 190 REM *** CHECK FOR CROSSOVERS (IE L(1)=L(2), ETC) ***         */
      /* 195 FOR J=1 TO 6                                     */
      /* 200 FOR K=1 TO 6                                     */
      /* 205 IF J=K THEN 215                                  */
      /* 210 IF L(J)=L(K) THEN 170                            */
      /* 215 NEXT K                                     */
      /* 220 NEXT J                                     */
      /* 225 REM *** SET NO. OF ARROWS ***                    */
      /* 230 A=5                                              */
      /* 235 L=L(1)                                     */
      w.scratchloc = w.you;

      /* 240 REM *** RUN THE GAME ***                         */
      /* 245 PRINT "HUNT THE WUMPUS"                          */
      prints(b"HUNT THE WUMPUS");

      while (w.finished == 0) {
            /* 250 REM *** HAZARD WARNING AND LOCATION ***                */
            /* 255 GOSUB 585                                        */
            w = check_hazards(w);

            /* 260 REM *** MOVE OR SHOOT ***                              */
            /* 265 GOSUB 670                                        */
            /* 270 ON O GOTO 280,300                                */
            let r = move_or_shoot(w);
            w = r.w;
            if (r.b) {
                  /* 275 REM *** SHOOT ***                              */
                  /* 280 GOSUB 715                                */
                  w = shoot(w);

                  /* 285 IF F=0 THEN 255                                */
                  /* 290 GOTO 310                                       */
            } else {
                  /* 295 REM *** MOVE ***                               */
                  /* 300 GOSUB 975                                */
                  w = move_you(w);

                  /* 305 IF F=0 THEN 255                                */
            };
      };

      /* 310 IF F>0 THEN 335                                  */
      if (w.finished == -1) {
            /* 315 REM *** LOSE ***                               */
            /* 320 PRINT "HA HA HA - YOU LOSE!"                   */
            /* 325 GOTO 340                                       */
            prints(b"HA HA HA - YOU LOSE!");
      } else {
            /* 330 REM *** WIN ***                                */
            /* 335 PRINT "HEE HEE HEE - THE WUMPUS'LL GET YOU NEXT TIME!!"    */
            prints(b"HEE HEE HEE - THE WUMPUS'LL GET YOU NEXT TIME!!");
      };
}

/* wumpus.c ends here */
