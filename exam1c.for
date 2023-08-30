***********************
*This program will allow users to play the game Midnight
*Rules are as follows
*Player 1 rolls six dice and must “bank” (select & reserve) at least one die after each roll. 
*Any dice not banked are rerolled.
*If a 1 or a 4 was not "banked" players score is 0.
*Else the score is the sum of all other dice rolled.
*Player 2 then repeats all of the steps above.
*The player with the highest score wins. 
*If the two players tie, the game begins again. 
*Players may be either computer or human.

*Britley Desir
*************************
      program Midnight
*Function/Shared variables
      integer findHigh, findOne, findFour, find6s, findSum
      integer dice1, dice2, dice3, dice4, dice5, dice6
      integer player1, player2, player1score, player2score
      integer player1turn, player2turn
*Computer variables
      integer rollArray(1:6)
      integer bank(1:6)
      integer bankPerRoll, oneCheck, fourCheck
      integer indexTracker, sixTracker
      integer count, i, j, k, l, m
*Human variables
      integer rollArray1(1:6)
      integer bank1(1:6)
      integer bankPerRoll1, oneCheck1, fourCheck1
      integer indexTracker1, sixTracker1
      integer bankFirst, bankSecond, bankThird, bankFourth
      integer bankFifth, bankSixth
*Declaring variables
      bankPerRoll = 0
      rollCounter = 0
      oneCheck = 0
      fourCheck = 0
      indexTracker = 1
      indexTracker1 = 1
      player1turn = 0
      player2turn = 0
      bank = (/0, 0, 0, 0, 0, 0/)
      bank1 = (/0, 0, 0, 0, 0, 0/)
*Randomly Randomizing the Randomizer
      call system_clock(count)
      call srand(count)

*Start of Game
      write(*,*) "Welcome to the game, Midnight."
  2   write(*,*) "Will Player 1 be a human or computer?"
      write(*,*) "Enter 1 for human or 2 for computer."
      read *, player1
      if (player1 .ne. 1 .and. player1 .ne. 2) then
         write(*,*) ""
         write(*,*) "You did not follow instructions."
         write(*,*) ""
         go to 2
      end if
      
  13  write(*,*) "Will Player 2 be a human or computer?"
      write(*,*) "Enter 1 for human or 2 for computer."
      read *, player2
      if (player2 .ne. 1 .and. player2 .ne. 2) then
         write(*,*) ""
         write(*,*) "You did not follow instructions."
         write(*,*) ""
         go to 13
      end if
      
      if (player1 .eq. 2) then
         go to 3
      else
         go to 4
      end if

*******************************
*                             *
*       CODE FOR HUMAN        *
*                             *
*******************************

  4   do while (indexTracker1 .ne. 7)
*Roll the dice and put them in an array
         dice1 = int(rand()*6 + 1)
         dice2 = int(rand()*6 + 1)
         dice3 = int(rand()*6 + 1)
         dice4 = int(rand()*6 + 1)
         dice5 = int(rand()*6 + 1)
         dice6 = int(rand()*6 + 1)
         rollArray1 = (/dice1, dice2, dice3, dice4, dice5, dice6/)

*Ensure appropriate number of dice are being rolled
         if (indexTracker1 .eq. 6) then
            write(*,*) "Dice Rolled"
            do j = 1, 1
               write(*,*)j,")", rollArray1(j)
            end do
         else if (indexTracker1 .eq. 5) then
            write(*,*) "Dice Rolled"
            do j = 1, 2
               write(*,*)j,")", rollArray1(j)
            end do
         else if (indexTracker1 .eq. 4) then
            write(*,*) "Dice Rolled"
            do j = 1, 3
               write(*,*)j,")", rollArray1(j)
            end do
         else if (indexTracker1 .eq. 3) then
            write(*,*) "Dice Rolled"
            do j = 1, 4
               write(*,*)j,")", rollArray1(j)
            end do
         else if (indexTracker1 .eq. 2) then
            write(*,*) "Dice Rolled"
            do j = 1, 5
               write(*,*)j,")", rollArray1(j)
            end do
         else if (indexTracker1 .eq. 1) then
            write(*,*) "Dice Rolled"
            do j = 1, 6
               write(*,*)j,")", rollArray1(j)
            end do
         end if

*Ask user how many dice they'd like to bank
  5      write(*,*) "How many dice would you like to bank?"
         read *, bankPerRoll1
         if (bankPerRoll1 .lt. 1 .or. bankPerRoll1 .gt. 6) then
            write(*,*) "Must bank no less than 1 and no more than 6."
            go to 5
         else if (indexTracker1 + bankPerRoll1 .gt. 7) then
            write(*,*) "You may only bank a total of 6 dice."
            go to 5
         end if

*Bank 1 die
  6      if (bankPerRoll1 .eq. 1) then
            write(*,*) "Which die?"
            read *, bankFirst
            if (bankFirst .gt. 1 .and. indexTracker1 .eq. 6) then
               write(*,*) "You can only choose 1."
               go to 6
            end if
*Ensure player is accessing appropriate dice            
            if (bankFirst .lt. 1 .or. bankFirst .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 6
            end if
            bank1(indexTracker1) = rollArray1(bankFirst)
            indexTracker1 = indexTracker1 + 1
         end if

*Bank 2 dice
  7      if (bankPerRoll1 .eq. 2) then
            write(*,*) "Which dice?"
            read *, bankFirst, bankSecond
            if (bankFirst .gt. 2 .and. indexTracker1 .eq. 5) then
               write(*,*) "You can choose 1-2."
               go to 7
            else if (bankFirst .gt. 2 .and. indexTracker1 .eq. 5) then
               write(*,*) "You can choose 1-2."
               go to 7
            end if
*Ensure player is accessing appropriate dice 
            if (bankFirst .lt. 1 .or. bankFirst .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 7
            else if (bankSecond .lt. 1 .or. bankSecond .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 7
            end if
            bank1(indexTracker1) = rollArray1(bankFirst)
            indexTracker1 = indexTracker1 + 1
            bank1(indexTracker1) = rollArray1(bankSecond)
            indexTracker1 = indexTracker1 + 1
         end if

*Bank 3 dice
  8      if (bankPerRoll1 .eq. 3) then
            write(*,*) "Which dice?"
            read *, bankFirst, bankSecond, bankThird
            if (bankFirst .gt. 3 .and. indexTracker1 .eq. 4) then
               write(*,*) "You can choose 1-3."
               go to 8
            else if (bankSecond .gt. 3 .and. indexTracker1 .eq. 4) then
               write(*,*) "You can choose 1-3."
               go to 8
            else if (bankThird .gt. 3 .and. indexTracker1 .eq. 4) then
               write(*,*) "You can choose 1-3."
               go to 8
            end if
*Ensure player is accessing appropriate dice 
            if (bankFirst .lt. 1 .or. bankFirst .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 8
            else if (bankSecond .lt. 1 .or. bankSecond .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 8
            else if (bankThird .lt. 1 .or. bankThird .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 8
            end if
            bank1(indexTracker1) = rollArray1(bankFirst)
            indexTracker1 = indexTracker1 + 1
            bank1(indexTracker1) = rollArray1(bankSecond)
            indexTracker1 = indexTracker1 + 1
            bank1(indexTracker1) = rollArray1(bankThird)
            indexTracker1 = indexTracker1 + 1
         end if

*Bank 4 dice
  9      if (bankPerRoll1 .eq. 4) then
            write(*,*) "Which dice?"
            read *, bankFirst, bankSecond, bankThird, bankFourth
            if (bankFirst .gt. 4 .and. indexTracker1 .eq. 3) then
               write(*,*) "You can choose 1-4."
               go to 9
            else if (bankSecond .gt. 4 .and. indexTracker1 .eq. 3) then
               write(*,*) "You can choose 1-4."
               go to 9
            else if (bankThird .gt. 4 .and. indexTracker1 .eq. 3) then
               write(*,*) "You can choose 1-4."
               go to 9
            else if (bankFourth .gt. 4 .and. indexTracker1 .eq. 3) then
               write(*,*) "You can choose 1-4."
               go to 9
            end if
*Ensure player is accessing appropriate dice 
            if (bankFirst .lt. 1 .or. bankFirst .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 9
            else if (bankSecond .lt. 1 .or. bankSecond .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 9
            else if (bankThird .lt. 1 .or. bankThird .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 9
            else if (bankFourth .lt. 1 .or. bankFourth .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 9
            end if
            bank1(indexTracker1) = rollArray1(bankFirst)
            indexTracker1 = indexTracker1 + 1
            bank1(indexTracker1) = rollArray1(bankSecond)
            indexTracker1 = indexTracker1 + 1
            bank1(indexTracker1) = rollArray1(bankThird)
            indexTracker1 = indexTracker1 + 1
            bank1(indexTracker1) = rollArray1(bankFourth)
            indexTracker1 = indexTracker1 + 1
         end if

*Bank 5 dice
  10     if (bankPerRoll1 .eq. 5) then
            write(*,*) "Which dice?"
            read *, bankFirst, bankSecond, bankThird, bankFourth
            read *, bankFifth
            if (bankFirst .gt. 5 .and. indexTracker1 .eq. 2) then
               write(*,*) "You can choose 1-5."
               go to 10
            else if (bankSecond .gt. 5 .and. indexTracker1 .eq. 2) then
               write(*,*) "You can choose 1-5."
               go to 10
            else if (bankThird .gt. 5 .and. indexTracker1 .eq. 2) then
               write(*,*) "You can choose 1-5."
               go to 10
            else if (bankFourth .gt. 5 .and. indexTracker1 .eq. 2) then
               write(*,*) "You can choose 1-5."
               go to 10
            else if (bankFifth .gt. 5 .and. indexTracker1 .eq. 2) then
               write(*,*) "You can choose 1-5."
               go to 10
            end if
*Ensure player is accessing appropriate dice 
            if (bankFirst .lt. 1 .or. bankFirst .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 10
            else if (bankSecond .lt. 1 .or. bankSecond .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 10
            else if (bankThird .lt. 1 .or. bankThird .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 10
            else if (bankFourth .lt. 1 .or. bankFourth .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 10
            else if (bankFifth .lt. 1 .or. bankFifth .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 10
            end if
            bank1(indexTracker1) = rollArray1(bankFirst)
            indexTracker1 = indexTracker1 + 1
            bank1(indexTracker1) = rollArray1(bankSecond)
            indexTracker1 = indexTracker1 + 1
            bank1(indexTracker1) = rollArray1(bankThird)
            indexTracker1 = indexTracker1 + 1
            bank1(indexTracker1) = rollArray1(bankFourth)
            indexTracker1 = indexTracker1 + 1
            bank1(indexTracker1) = rollArray1(bankFifth)
            indexTracker1 = indexTracker1 + 1
         end if

*Bank 6 dice
  11     if (bankPerRoll1 .eq. 6) then
            write(*,*) "Which dice?"
            read *, bankFirst, bankSecond, bankThird, bankFourth
            read *, bankFifth, bankSixth
            if (bankFirst .gt. 6 .and. indexTracker1 .eq. 1) then
               write(*,*) "You can choose 1-6."
               go to 11
            else if (bankSecond .gt. 6 .and. indexTracker1 .eq. 1) then
               write(*,*) "You can choose 1-6."
               go to 11
            else if (bankThird .gt. 6 .and. indexTracker1 .eq. 1) then
               write(*,*) "You can choose 1-6."
               go to 11
            else if (bankFourth .gt. 6 .and. indexTracker1 .eq. 1) then
               write(*,*) "You can choose 1-6."
               go to 11
            else if (bankFifth .gt. 6 .and. indexTracker1 .eq. 1) then
               write(*,*) "You can choose 1-6."
               go to 11
            else if (bankSixth .gt. 6 .and. indexTracker1 .eq. 1) then
               write(*,*) "You can choose 1-6."
               go to 11
            end if
*Ensure player is accessing appropriate dice 
            if (bankFirst .lt. 1 .or. bankFirst .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 11
            else if (bankSecond .lt. 1 .or. bankSecond .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 11
            else if (bankThird .lt. 1 .or. bankThird .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 11
            else if (bankFourth .lt. 1 .or. bankFourth .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 11
            else if (bankFifth .lt. 1 .or. bankFifth .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 11
            else if (bankSixth .lt. 1 .or. bankSixth .gt. 6) then
               write(*,*) "Your options are 1 - 6."
               go to 11
            end if
            bank1(indexTracker1) = rollArray1(bankFirst)
            indexTracker1 = indexTracker1 + 1
            bank1(indexTracker1) = rollArray1(bankSecond)
            indexTracker1 = indexTracker1 + 1
            bank1(indexTracker1) = rollArray1(bankThird)
            indexTracker1 = indexTracker1 + 1
            bank1(indexTracker1) = rollArray1(bankFourth)
            indexTracker1 = indexTracker1 + 1
            bank1(indexTracker1) = rollArray1(bankFifth)
            indexTracker1 = indexTracker1 + 1
            bank1(indexTracker1) = rollArray1(bankSixth)
            indexTracker1 = indexTracker1 + 1
         end if
  12     continue
      end do

*Printing Final Bank for players
      if (player1turn .eq. 0) then
         write(*,*) "********************************"
         write(*,*) "Player 1 Bank"
         do j = 1, 6
            write(*,*) bank1(j)
         end do
         write(*,*) "********************************"
      else
         write(*,*) "********************************"
         write(*,*) "Player 2 Bank"
         do j = 1, 6
            write(*,*) bank1(j)
         end do
         write(*,*) "********************************"
      end if

*Calculating players scores and switching players/ending game
      if (player1turn .eq. 0) then
         player1turn = 1
         if (findOne(bank1) .ne. 1 .or. findFour(bank1) .ne. 4) then
            player1score = 0
         else
            player1score = findSum(bank1) - 5
         end if

         if (player2turn .eq. 0 .and. player2 .eq. 1) then
            bankPerRoll1 = 0
            oneCheck1 = 0
            fourCheck1 = 0
            indexTracker1 = 1
            bank1 = (/0, 0, 0, 0, 0, 0/)
            go to 4
         else if (player2turn .eq. 0 .and. player2 .eq. 2) then
            bankPerRoll = 0
            oneCheck = 0
            fourCheck = 0
            indexTracker = 1
            bank = (/0, 0, 0, 0, 0, 0/)
            go to 3
         end if
      else
         player2turn = 1
         if (findOne(bank1) .ne. 1 .and. findFour(bank1) .ne. 4) then
            player2score = 0
         else
            player2score = findSum(bank1) - 5
         end if
         go to 1
      end if

*******************************
*                             *
*     CODE FOR COMPUTER       *
*                             *
*******************************

  3   do while (indexTracker .ne. 7)
*Roll dice, store values in rollArray, increase rollCounter by 1
         dice1 = int(rand()*6 + 1)
         dice2 = int(rand()*6 + 1)
         dice3 = int(rand()*6 + 1)
         dice4 = int(rand()*6 + 1)
         dice5 = int(rand()*6 + 1)
         dice6 = int(rand()*6 + 1)
*Account for dice banked         
         if (indexTracker .eq. 6) then
            rollArray = (/dice1, 0, 0, 0, 0, 0/)
         else if (indexTracker .eq. 5) then
            rollArray = (/dice1, dice2, 0, 0, 0, 0/)
         else if (indexTracker .eq. 4) then
            rollArray = (/dice1, dice2, dice3, 0, 0, 0/)
         else if (indexTracker .eq. 3) then
            rollArray = (/dice1, dice2, dice3, dice4, 0, 0/)
         else if (indexTracker .eq. 2) then
            rollArray = (/dice1, dice2, dice3, dice4, dice5, 0/)
         else if (indexTracker .eq. 1) then
            rollArray = (/dice1, dice2, dice3, dice4, dice5, dice6/)
         end if

*Bank a 1 if rolled and do not count 1's twice
         if (findOne(rollArray) .eq. 1 .and. oneCheck .eq. 0) then
            bank(indexTracker) = 1
            oneCheck = 1
            bankPerRoll = bankPerRoll + 1
            indexTracker = indexTracker + 1
            
            if (indexTracker .eq. 7) then
               go to 14
            end if
         end if

*Bank a 4 if rolled and do not count 4's twice
         if (findFour(rollArray) .eq. 4 .and. fourCheck .eq. 0) then
            bank(indexTracker) = 4
            fourCheck = 1
            bankPerRoll = bankPerRoll + 1
            indexTracker = indexTracker + 1
            
            if (indexTracker .eq. 7) then
               go to 14
            end if
         end if

*Bank any 6's rolled
         if (find6s(rollArray) .gt. 0) then
            sixTracker = find6s(rollArray)
            do while (sixTracker .ne. 0)
               bank(indexTracker) = 6
               bankPerRoll = bankPerRoll + 1
               indexTracker = indexTracker + 1
               sixTracker = sixTracker - 1
            end do
            
            if (indexTracker .eq. 7) then
               go to 14
            end if
         end if

*Bank the highest number if nothing has been banked this roll
         if (bankPerRoll .eq. 0) then
            bank(indexTracker) = findHigh(rollArray)
            bankPerRoll = bankPerRoll + 1
            indexTracker = indexTracker + 1
            
            if (indexTracker .eq. 7) then
               go to 14
            end if
         end if    

*Resetting bankPerRoll at the end of each roll         
         bankPerRoll = 0
      end do

*Printing Final Bank for players
  14  if (player1turn .eq. 0) then
         write(*,*) "********************************"
         write(*,*) "Player 1 Bank"
         do j = 1, 6
            write(*,*) bank(j)
         end do
      else
         write(*,*) "********************************"
         write(*,*) "Player 2 Bank"
         do j = 1, 6
            write(*,*) bank(j)
         end do
      end if

*Calculating players scores and switching players/ending game
      if (player1turn .eq. 0) then
         player1turn = 1
         if (findOne(bank) .ne. 1 .or. findFour(bank) .ne. 4) then
            player1score = 0
         else
            player1score = findSum(bank) - 5
         end if

         if (player2turn .eq. 0 .and. player2 .eq. 1) then
            bankPerRoll1 = 0
            oneCheck1 = 0
            fourCheck1 = 0
            indexTracker1 = 1
            bank1 = (/0, 0, 0, 0, 0, 0/)
            go to 4
         else if (player2turn .eq. 0 .and. player2 .eq. 2) then
            bankPerRoll = 0
            oneCheck = 0
            fourCheck = 0
            indexTracker = 1
            bank = (/0, 0, 0, 0, 0, 0/)
            go to 3
         end if
      else
         player2turn = 1
         if (findOne(bank) .ne. 1 .and. findFour(bank) .ne. 4) then
            player2score = 0
         else
            player2score = findSum(bank) - 5
         end if
         go to 1
      end if

*Print results/starting over if its a tie
  1   write(*,*) "###################################"
      write(*,*) "Player 1 score", player1score
      write(*,*) "Player 2 score", player2score
      if (player1score .gt. player2score) then
         write(*,*) "Player 1 is the winner."
      else if (player2score .gt. player1score) then
         write(*,*) "Player 2 is the winner."
      else if (player1score .eq. player2score) then
         write(*,*) "There is a tie. You must play again."
         player1turn = 0
         player2turn = 0
         go to 2
      end if
      write(*,*) "###################################"

      STOP
      END

*Functions
      function findHigh(a)
         integer findHigh, i
         integer a(1:6)
         findHigh = 0

         do i = 1,6
            if (a(i) .gt. findHigh) then
               findHigh = a(i)
            end if
         end do
      return
      end

      function findOne(a)
         integer findOne, i
         integer a(1:6)
         findOne = 0

         do i = 1,6
            if (a(i) .eq. 1) then
               findOne = 1
            end if
         end do
      return
      end

      function findFour(a)
         integer findFour, i
         integer a(1:6)
         findFour = 0

         do i = 1,6
            if (a(i) .eq. 4) then
               findFour = 4
            end if
         end do
      return
      end

      function findSum(a)
         integer findSum, i
         integer a(1:6)
         findSum = 0

         do i = 1,6
            findSum = a(i) + findSum
         end do
      return
      end

      function find6s(a)
         integer i, find6s
         integer a(1:6)
         find6s = 0

         do i = 1,6
            if(a(i) .eq. 6) then
               find6s = find6s + 1
            end if
         end do
      return
      end