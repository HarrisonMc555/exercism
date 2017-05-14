import re

def board(text):
    if not is_valid_board(text):
        raise ValueError('Not a valid board')
    board = extract_board(text)
    for r in range(len(board)):
        for c in range(len(board[0])):
            if board[r][c] == ' ':
                # print('board[{}][{}] = '.format(r, c), end='')
                # print(num_neighboring_mines(board, r, c))
                num_mines = num_neighboring_mines(board, r, c)
                if num_mines > 0:
                    board[r][c] = str(num_mines)
    return output_board(board)

def num_neighboring_mines(board, row, col):
    min_row = row-1 if row != 0 else 0
    max_row = row+1 if row != len(board)-1 else len(board)-1
    min_col = col-1 if col != 0 else 0
    max_col = col+1 if col != len(board[0])-1 else len(board[0])-1
    num_mines = 0
    for r in range(min_row, max_row+1):
        for c in range(min_col, max_col+1):
            if not (r == row and c == col):
                if board[r][c] == '*':
                    num_mines += 1
    return num_mines
    

def extract_board(text):
    return [list(row[1:-1]) for row in text[1:-1]]

def output_board(board):
    num_cols = len(board[0])
    border = '+{}+'.format('-'*(num_cols))
    def output_row(row):
        return '|{}|'.format(''.join(row))
    return [border] + [output_row(row) for row in board] + [border]

def is_valid_board(text):
    if type(text) != type([]): return False
    if len(text) < 3: return false
    lenFirstRow = len(text[0])
    if not all([len(row) == lenFirstRow for row in text]): return False
    border_pattern = '\++'
    if not (re.match(border_pattern, text[0]) and
            re.match(border_pattern, text[-1])): return False
    row_pattern = '\|[ *]+\|'
    if not all([re.match(row_pattern, row)
                for row in text[1:-1]]): return False
    return True

# board1 = ["+------+",
#           "| *  * |",
#           "|  *   |",
#           "|    * |",
#           "|   * *|",
#           "| *  * |",
#           "|      |",
#           "+------+"]
# board2 = ["+-----+",
#           "| * * |",
#           "|     |",
#           "|   * |",
#           "|  * *|",
#           "| * * |",
#           "+-----+"]
# board3 = ["+-----+",
#           "| * * |",
#           "+-----+"]
# board4 = ["+-+",
#           "|*|",
#           "| |",
#           "|*|",
#           "| |",
#           "| |",
#           "+-+"]
# board5 = ["+-+",
#           "|*|",
#           "+-+"]
# board6 = ["+--+",
#           "|**|",
#           "|**|",
#           "+--+"]
# board7 = ["+--+",
#           "|**|",
#           "|**|",
#           "+--+"]
# bad1 = ["+-+",
#         "| |",
#         "|*  |",
#         "|  |",
#         "+-+"]
# bad2 = ["+-----+",
#         "*   * |",
#         "+-- --+"]
# bad3 = ["+-----+",
#         "|X  * |",
#         "+-----+"]
# print(is_valid_board(board1))
# print(is_valid_board(board2))
# print(is_valid_board(board3))
# print(is_valid_board(board4))
# print(is_valid_board(board5))
# print(is_valid_board(board6))
# print(is_valid_board(bad1))
# print(is_valid_board(bad2))
# print(is_valid_board(bad3))
# print(extract_board(board1))
# print(extract_board(board2))
# print(extract_board(board3))
# print(extract_board(board4))
# print(extract_board(board5))
# print(extract_board(board6))
# print(output_board(extract_board(board1)))
# print(output_board(extract_board(board2)))
# print(output_board(extract_board(board3)))
# print(output_board(extract_board(board4)))
# print(output_board(extract_board(board5)))
# print(output_board(extract_board(board6)))
# print(board(board1))
# print(board(board2))
# print(board(board3))
# print(board(board4))
# print(board(board5))
# print(board(board6))
