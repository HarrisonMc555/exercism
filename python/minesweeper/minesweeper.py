import re

def board():
    pass

def extract_board(text):
    return [row[1:-1] for row in text[1:-1]]

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

board1 = ["+------+",
          "| *  * |",
          "|  *   |",
          "|    * |",
          "|   * *|",
          "| *  * |",
          "|      |",
          "+------+"]
board2 = ["+-----+",
          "| * * |",
          "|     |",
          "|   * |",
          "|  * *|",
          "| * * |",
          "+-----+"]
board3 = ["+-----+",
          "| * * |",
          "+-----+"]
board4 = ["+-+",
          "|*|",
          "| |",
          "|*|",
          "| |",
          "| |",
          "+-+"]
board5 = ["+-+",
          "|*|",
          "+-+"]
board6 = ["+--+",
          "|**|",
          "|**|",
          "+--+"]
board7 = ["+--+",
          "|**|",
          "|**|",
          "+--+"]
bad1 = ["+-+",
        "| |",
        "|*  |",
        "|  |",
        "+-+"]
bad2 = ["+-----+",
        "*   * |",
        "+-- --+"]
bad3 = ["+-----+",
        "|X  * |",
        "+-----+"]
print(is_valid_board(board1))
print(is_valid_board(board2))
print(is_valid_board(board3))
print(is_valid_board(board4))
print(is_valid_board(board5))
print(is_valid_board(board6))
print(is_valid_board(bad1))
print(is_valid_board(bad2))
print(is_valid_board(bad3))
print(extract_board(board1))
print(extract_board(board2))
print(extract_board(board3))
print(extract_board(board4))
print(extract_board(board5))
print(extract_board(board6))
