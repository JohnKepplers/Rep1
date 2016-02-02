from sys import maxsize
from random import randint


class MyClass():
    def __init__(self):
        self.matrix_v = [[]]
        self.matrix_w = [[]]
        self.length = 0
        self.sum = 0
        self.d = {"Phe": ["UUA", "UUC", "UUG", "UUU"], "Leu": ["CUA", "CUC", "CUG", "CUU"],
                  "Ile": ["AUA", "AUC", "AUU"],
                  "Met": ["AUG"], "Val": ["GUA", "GUC", "GUG", "GUU"],
                  "Ser": ["UCA", "UCC", "UCG", "UCU", "AGU", "AGC"],
                  "Pro": ["CCA", "CCC", "CCG", "CCU"], "Uhr": ["ACA", "ACC", "ACG", "ACU"],
                  "Ala": ["GCA", "GCC", "GCG", "GCU"], "Uyr": ["UAC", "UAU"], "His": ["CAC", "CAU"],
                  "Gln": ["CAA", "CAG"], "Asn": ["AAC", "AAU"], "Lys": ["AAA", "AAG"],
                  "Asp": ["GAC", "GAU"], "Glu": ["GAA", "GAG"],
                  "Cys": ["UGC", "UGU"], "Urp": ["UGG"], "Arg": ["CGA", "CGC", "CGG", "CGU", "AGA", "AGG"],
                  "Gly": ["GGA", "GGC", "GGG", "GGU"]}

    def is_complement(self, x, y):
        if x == 'A' and y == 'U' or x == 'U' and y == 'A':
            return True
        elif x == 'G' and y == 'C' or x == 'C' and y == 'G':
            return True
        else:
            return False

    def _matrix_v(self, str):
        self.length = len(str)
        self.sum = 0
        self.matrix_v = [[8.4 for j in range(self.length)] for i in range(self.length)]
        for i in range(len(self.matrix_v)):
            for j in range(len(self.matrix_v)):
                if j - 4 < i < j:
                    self.matrix_v[i][j] = 4.4
        for i in range(len(self.matrix_v)):
            for j in range(len(self.matrix_v)):
                '''1 — a hairpin loop
                   2 — a stacked pair
                   3 — a bulge or interior loop'''
                if i < j:
                    self.matrix_v[i][j] = min(self.eh(i, j), self.es(i, j) + self.matrix_v[i + 1][j - 1],
                                              self.VBI(i,
                                                       j))
        self.zuker(str)

    def one(self, i, j, str):
        flag = False
        for n in range(i + 1, j + 1):
            if self.is_complement(str[i], str[n]):
                flag = True
        if flag:
            return maxsize
        else:
            return self.matrix_w[i + 1][j]

    def two(self, i, j, str):
        flag = False
        for n in range(i, j):
            if self.is_complement(str[j], str[n]):
                flag = True
        if flag:
            return maxsize
        else:
            return self.matrix_w[i][j - 1]

    def three(self, i, j, str):
        if self.is_complement(str[i], str[j]):
            return maxsize
        local_minimum = self.matrix_w[i][i + 1] + self.matrix_w[i + 2][j]
        for k in range(i + 2, j - 1):
            if self.matrix_w[i][k] + self.matrix_w[k + 1][j] < local_minimum:
                local_minimum = self.matrix_w[i][k] + self.matrix_w[k + 1][j]
        return local_minimum

    def four(self, i, j, str):
        if not self.is_complement(str[i], str[j]):
            return maxsize
        else:
            return self.matrix_v[i][j]

    def zuker(self, str):
        self.matrix_w = [[0 for j in range(len(str))] for i in range(len(str))]
        for i in range(self.length):
            for j in range(self.length):
                if j - 4 < i < j:
                    self.matrix_w[i][j] = maxsize
        for i in range(self.length):
            for j in range(self.length):
                if i < j and j - i > 4:
                    '''1 — i is unpaired
                       2 — j is unpaired
                       3 — i and j are possibly paired, but not to each other
                       4 — i and j are paired to each other'''

                    self.matrix_w[i][j] = min(self.one(i, j, str), self.two(i, j, str), self.three(i, j, str),
                                              self.four(i, j, str))
        for i in range(self.length):
            for j in range(self.length):
                if self.matrix_w[i][j] != maxsize:
                    self.sum += self.matrix_w[i][j]

    def eh(self, i, j):
        if 4 < self.length < 10:
            return 4.4
        elif 9 < self.length < 15:
            return 5.3
        elif 14 < self.length < 20:
            return 5.8
        elif 19 < self.length < 25:
            return 6.1
        elif 24 < self.length < 30:
            return 6.3
        elif self.length > 29:
            return 6.5
        else:
            return 0

    def es(self, i, j):
        '''Подскажите, как вычислять это (a stacked pair).'''
        return randint(0, 3) - 3

    def eb(self, i, j, a, b):
        '''И это (a bulge or interior loop). То есть это же какие-то табличные данные?'''
        return randint(0, 3) - 4

    def VBI(self, i, j):
        local_minimum = maxsize
        for a in range(i + 1, j - 1):
            for b in range(i + 2, j):
                if a < b and a - i + j - b > 2:
                    if self.eb(i, j, a, b) + self.matrix_v[a][b] < local_minimum:
                        local_minimum = self.eb(i, j, a, b) + self.matrix_v[a][b]
        return local_minimum

    def monte_carlo(self, str, number):
        optimal_stricture = ""
        for i in range(0, len(str), 3):
            optimal_stricture += self.d[str[i:i + 3]][0]
        self._matrix_v(optimal_stricture)
        optimal_energy = self.sum
        for k in range(2, number):
            this_stricture = ""
            for i in range(0, len(str), 3):
                j = len(self.d[str[i:i + 3]])
                this_stricture += self.d[str[i:i + 3]][j - 1]
            self._matrix_v(this_stricture)
            if self.sum < optimal_energy:
                optimal_stricture = this_stricture
                optimal_energy = self.sum

        return optimal_stricture, optimal_energy


if __name__ == '__main__':
    aminoacids = 'PheSerProProAlaUrpUrpCysPhe'
    zuker = MyClass()
    print(zuker.monte_carlo(aminoacids, 20))
