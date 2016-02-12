program Tictactoe

    character * 1 tictac(3,3), winner
    data tictac / " ", " ",  " ",  " ",  " ",  " ",  " ",  " ",  " " /

    integer :: move, turn
    logical :: chkinput, over

    character(len=36) :: format
    format = "(2X,A1,1X,'|',1X,A1,1X,'|',1X,A1,1X)"

    write(*,*) "PLAY TIC-TAC-TOE. ENTER 1-9 TO PLAY"
    write(*,*) " "
    write(*,*) " 1 | 2 | 3 "
    write(*,*) " ---+---+---"
    write(*,*) " 4 | 5 | 6 "
    write(*,*) " ---+---+---"
    write(*,*) " 7 | 8 | 9 "
    write(*,*) " "

    do
        call getinput(turn, move);

        if (chkinput(move, tictac)) then

            call printboard(tictac, format, turn)

            if (turn == 1) then
                call chkovr(tictac, over, winner)

                if (over) then
                    write(*,*) "The game is over!"
                    
                    if (winner == "D") then
                        write(*,*) "The game is a draw. "
                    else
                        write(*,*) "The winner is: ", WINNER
                    end if
                    stop
                else
                    cycle
                end if
            end if

            turn = 1
            
            call compmove(tictac)
            call printboard(tictac, format, turn)

            call chkovr(tictac, over, winner)
            if (over) then
                write(*,*) "The game is over!"
                
                if (winner == "D") then
                    write(*,*) "The game is a draw. "
                else
                    write(*,*) "The winner is: ", WINNER
                end if
                stop
            else
                cycle
            end if
            exit
        end if
    end do
end

subroutine compmove(tictac)
    character * 1 tictac(3,3)
    integer :: paths(3,8), pathsum(8)
    data paths /1,2,3,4,5,6,7,8,9,1,4,7,2,5,8,3,6,9,1,5,9,3,5,7/
    integer :: board(9,2), k, x, y, randpos
    data board /1,1,1,2,2,2,3,3,3,1,2,3,1,2,3,1,2,3/

    do i = 1, 8
        pathsum(i) = 0
        do j = 1, 3
            x = board(paths(j, i),1)
            y = board(paths(j, i),2)
            if (tictac(x,y) == " ") then
                k = 0
            else if (tictac(x,y) == "X") then
                k = 1
            else if (tictac(x,y) == "O") then
                k = 4
            end if
            pathsum(i) = pathsum(i) + k
        end do
    end do

    do i = 1, 8
        if (pathsum(i) == 8) then
            do j = 1, 3
                x = board(paths(j, i),1)
                y = board(paths(j, i),2)
                if (tictac(x,y) == " ") then
                    tictac(x,y) = "O"
                    return
                end if
            end do
        end if
    end do

    do i = 1, 8
        if (pathsum(i) == 2) then
            do j = 1, 3
                x = board (paths(j, i),1)
                y = board (paths(j, i),2)
                if (tictac(x,y) == " ") then
                    tictac(x, y) = "O"
                    return
                end if
            end do
        end if
    end do

    do i = 0,9
        randpos = int(rand(0)*9)+1
        x = board(randpos, 1)
        y = board(randpos, 2)
        if (tictac(x, y) == " ") then
            tictac(x,y) = "O"
            return
        end if
    end do

    return
end

subroutine chkovr(tictac, over, winner)
    character * 1 tictac(3,3), winner
    logical :: over

    character * 1 blank, draw
    parameter (blank = ' ', draw = 'D')

    logical :: same
    logical :: dsame
    integer :: ir, ic

    over = .true.

    do ir = 1, 3
        if (same(tictac(ir, 1), tictac(ir, 2), tictac(ir, 3))) then
            winner = tictac(ir, 1)
            return
        end if
    end do

    do ic = 1, 3
        if (same(tictac(1, ic), tictac(2, ic), tictac(3, ic))) then
            winner = tictac(1, ic)
            return
        end if
    end do

    dsame = same(tictac(1,1),tictac(2,2),tictac(3,3)) .or. same(tictac(1,3),tictac(2,2),tictac(3,1))

    if (dsame) then
        winner = tictac(2,2)
        return
    end if

    do ir = 1, 3
        do ic = 1, 3
            if (tictac(ir, ic) == blank) then
                over = .false.
                return
            end if
        end do
    end do

    winner = draw

    return
end

logical function same(t1, t2, t3)
    character t1, t2, t3

    if (t1 == "X" .and. t2 == "X" .and. t3 == "X") then
        same = .true.
    else if (t1 == "O" .and. t2 == "O" .and. t3 == "O") then
        same = .true.
    else
        same = .false.
    end if
end

subroutine printboard(tictac, format, turn)

    character * 1 tictac(3,3)
    character(len=36) :: format
    integer :: turn

    if (turn == 0) write(*,*) "After your move..."
    if (turn == 1) write(*,*) "After my move..."

    do i = 1,3
        write(*,format) (tictac(i,j), j=1,3)
        if (i < 3) then
                write(*,*) "---+---+---"
        end if
    end do
end

subroutine getinput(turn, move)
    integer move, turn

    turn = 0
    write(*,*) "Your move? "
    read(*,*) move
end

logical function chkinput(move, tictac)

    character * 1 tictac(3,3)
    integer :: move
    logical :: chkplay
    
    if (move > 0 .and. move <=9) then
        if (chkplay(tictac, move)) then
            chkinput = .true.
        else
            chkinput = .false.
            write(*,*) "Invalid move, box already occupied."
        end if
    else
        chkinput = .false.
        write(*,*) "Invalid input."
    end if

end


logical function chkplay(tictac, move)
    character * 1 tictac(3,3)
    integer :: move

    select case (move)
        case (1)
            if (tictac(1,1) == " ") then
                chkplay = .true.
                tictac(1,1) = "X"
            else
                chkplay = .false.
            end if
        case (2)
            if (tictac(1,2) == " ") then
                chkplay = .true.
                tictac(1,2) = "X"
            else
                chkplay = .false.
            end if
        case (3)
            if (tictac(1,3) == " ") then
                chkplay = .true.
                tictac(1,3) = "X"
            else
                chkplay = .false.
            end if
        case (4)
            if (tictac(2,1) == " ") then
                chkplay = .true.
                tictac(2,1) = "X"
            else
                chkplay = .false.
            end if
        case (5)
            if (tictac(2,2) == " ") then
                chkplay = .true.
                tictac(2,2) = "X"
            else
                chkplay = .false.
            end if
        case (6)
            if (tictac(2,3) == " ") then
                chkplay = .true.
                tictac(2,3) = "X"
            else
                chkplay = .false.
            end if
        case (7)
            if (tictac(3,1) == " ") then
                chkplay = .true.
                tictac(3,1) = "X"
            else
                chkplay = .false.
            end if
        case (8)
            if (tictac(3,2) == " ") then
                chkplay = .true.
                tictac(3,2) = "X"
            else
                chkplay = .false.
            end if
        case (9)
            if (tictac(3,3) == " ") then
                chkplay = .true.
                tictac(3,3) = "X"
            else
                chkplay = .false.
            end if
    end select
end
