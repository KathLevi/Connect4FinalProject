#include<iostream>
#include<string>
//////////////////////////////////////////////
///// ENTIRE PROJECT DONE IN ASSEMBLER ///////
//////////////////////////////////////////////

char board[6][7];
///display the board on the consol
void displayBoard() {
	std::cout << " 1   2   3   4   5   6   7\n";		//keeps track of columns
														//displays board to screen
	for (int i = 0; i < 6; i++) {
		for (int j = 0; j < 7; j++)										//creates each box in thirds using ASCII characters
			std::cout << char(218) << char(196) << char(191) << " ";	//top 1/3
		std::cout << std::endl;											//
		for (int j = 0; j < 7; j++)										//
			std::cout << char(179) << board[i][j] << char(179) << " ";	//middle 2/3
		std::cout << std::endl;											//
		for (int j = 0; j < 7; j++)										//
			std::cout << char(192) << char(196) << char(217) << " ";	//bottom 3/3
		std::cout << std::endl;
	}
}

///initalize the game board to all blank spaces
void emptyBoard() {
	for (int i = 0; i < 6; i++) {
		for (int j = 0; j < 7; j++) {
			board[i][j] = ' ';
		}
	}
}

///placing a piece on the board
int placeTile(int col, char player) {
	int i;
	if (col >= 0 && col < 7) {
		if (board[0][col] == ' ') {
			for (i = 0; board[i][col] == ' '; i++)
				if (i == 5) {
					board[i][col] = player;
					return i;
				}
			i--;
			board[i][col] = player;
			return i;
		}
		else
			return -1;
	}
	else
		return -1;
}

///check to see if someone has one yet by checking
///horizontally, vertically, and both wasy diagonally across the whole board
bool check(int curCol, int curRow) {
	int inarow = 1;
	int c = curCol;
	int r = curRow;
	//check horizontal
	//go left
	while (c > 0) {
		if (board[curRow][curCol] == board[r][c - 1]) {
			inarow++;
			if (inarow == 4) {
				return true;
			}
		}
		else {
			break;
		}
		c--;
	}
	c = curCol;
	//go right
	while (c < 6) {
		if (board[curRow][curCol] == board[r][c + 1]) {
			inarow++;
			if (inarow == 4) {
				return true;
			}
		}
		else {
			break;
		}
		c++;
	}
	inarow = 1;
	c = curCol;

	//check vertical
	//go up
	while (r > 0) {
		if (board[curRow][curCol] == board[r - 1][c]) {
			inarow++;
			if (inarow == 4) {
				return true;
			}
		}
		else {
			break;
		}
		r--;
	}
	r = curRow;

	//go down
	while (r < 5) {
		if (board[curRow][curCol] == board[r + 1][c]) {
			inarow++;
			if (inarow == 4) {
				return true;
			}
		}
		else {
			break;
		}
		r++;
	}
	inarow = 1;
	r = curRow;

	//check diagonally (/)
	//go up-right
	while (c < 6 && r > 0) {
		if (board[curRow][curCol] == board[r - 1][c + 1]) {
			inarow++;
			if (inarow == 4) {
				return true;
			}
		}
		else {
			break;
		}
		r--;
		c++;
	}
	r = curRow;
	c = curCol;

	//go down-left
	while (c > 0 && r < 5) {
		if (board[curRow][curCol] == board[r + 1][c - 1]) {
			inarow++;
			if (inarow == 4) {
				return true;
			}
		}
		else {
			break;
		}
		r++;
		c--;
	}
	inarow = 1;
	r = curRow;
	c = curCol;

	//check diagonally (\)
	//go up-left
	while (c > 0 && r > 0) {
		if (board[curRow][curCol] == board[r - 1][c - 1]) {
			inarow++;
			if (inarow == 4) {
				return true;
			}
		}
		else {
			break;
		}
		r--;
		c--;
	}
	r = curRow;
	c = curCol;

	//go down-right
	while (c < 6 && r < 5) {
		if (board[curRow][curCol] == board[r + 1][c + 1]) {
			inarow++;
			if (inarow == 4) {
				return true;
			}
		}
		else {
			break;
		}
		r++;
		c++;
	}
	inarow = 1;
	r = curRow;
	c = curCol;

	return false;
}

int main() {
	char again = 121;
	std::string pl1, pl2;
	std::cout << "\nPlayer 1: What would you like to be known as? \n";
	std::cin >> pl1;
	std::cout << "\nPlayer 2: What would you like to be known as? \n";
	std::cin >> pl2;
	system("cls");

	do {
		emptyBoard();
		displayBoard();		//displays a blank board
		int rowChoice, curCol = 0, placed = 0;
		bool win = false;
		char player = 15;
		do {
			if (curCol != -1) {
				if (player == 15) {
					std::cout << pl1 << ", where would you like to drop your tile?";
					player = 254;
				}
				else {
					std::cout << pl2 << ", where would you like to drop your tile?";
					player = 15;
				}
			}

			while (true) {
				if (placed == 42)
					break;
				std::cin >> rowChoice;
				rowChoice--;
				if (rowChoice < 7 && rowChoice >= 0)
					break;
				else
					std::cout << "\nPlease enter a value between 1 and 7 : ";
				if (std::cin.fail())
				{
					std::cin.clear();
					char c;
					std::cin >> c;
				}
			}
			if (placed == 42)
				break;

			curCol = placeTile(rowChoice, player);
			if (curCol == -1)
				std::cout << "Column is full\nPlease choose a differnt column: ";
			else {
				if (placed >= 6)
					win = check(rowChoice, curCol);	

				placed++;
				system("cls");
				displayBoard();
			}
		} while (!win);

		system("cls");
		displayBoard();
		if (placed == 42) {
			std::cout << "IT'S A DRAW" << std::endl;
			system("pause");
		}
		if (player == 15)
			std::cout << pl2 << " won! " << std::endl;
		else
			std::cout << pl1 << " won! " << std::endl;
		system("pause");
		std::cout << "Would you like to play again? (Y or N) ";
		std::cin >> again;
	} while (tolower(again) == 'y');

	system("cls");
	std::cout << "Thank you for playing Connect 4! Come and play again soon (:" << std::endl;
	system("pause");

	return 0;
}
