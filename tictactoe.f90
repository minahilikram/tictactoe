program Tictactoe

	character * 1 tictac(3,3)
	data tictac / " ", " ",  " ",  " ",  " ",  " ",  " ",  " ",  " " /

	integer move, turn
	logical chkplay, chkinput

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

	move = 0

	do
		call getinput(turn, move);

		if (chkinput(move, tictac)) then

			if (turn == 0) write(*,*) "After your move..."
			if (turn == 1) write(*,*) "After my move..."

			do i = 1,3
				write(*,format) (tictac(i,j), j=1,3)
				write(*,*) "---+---+---"
			end do

			exit
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
	integer move
	logical chkplay
	
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
	integer move

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
