//
// Instruction memory: 32k of 48-bit words.
//
// Copyright (c) 2019 Serge Vakulenko
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
`default_nettype none

module imemory(
    input  wire         clk,            // clock
    input  wire  [14:0] i_addr,         // address input
    input  wire         i_read,         // read op
    output logic [47:0] o_data,         // data from memory
    output logic        o_done          // write op
);

// Global time parameters.
timeunit 1ns / 1ps;

logic [47:0] mem[32*1024];              // main RAM 32k words

always @(posedge clk) begin
    if (i_read & ~o_done) begin
        o_data <= mem[i_addr];          // memory load
    end

    o_done <= i_read;
end

initial begin
    //
    // Пультовая программа #1: тест управления.
    // Ограниченная проверка УУ без ОЗУ и АУ.
    //
    // Проверка выполнения отдельных операций, связанных
    // с модификаторами. Проверка модификаторов М1, М2, МЗ и
    // сумматора адреса в режиме счетчика путем сложения
    // прямого и дополнительного кодов.
    //
    // При останове в результате ненулевой суммы при сложении в M1
    // находится прямой, а в М2 дополнительный код, давшие
    // ненулевую сумму.
    //
    mem[1] = {4'o02, 5'o24, 15'o00000, 4'o01, 5'o31, 15'o00002}; // начало: уиа  0(2)   пв   повтор(1)
    mem[2] = {4'o01, 5'o25, 15'o77777, 4'o02, 5'o25, 15'o77777}; // повтор: слиа -1(1)  слиа -1(2)
    mem[3] = {4'o01, 8'o044, 12'o0003, 4'o03, 5'o34, 15'o00007}; //         уии  3(1)   пио  ошибка(3)
    mem[4] = {4'o02, 8'o045, 12'o0000, 4'o03, 5'o35, 15'o00007}; //         сли  (2)    пино ошибка(3)
    mem[5] = {4'o01, 5'o25, 15'o00001, 4'o01, 5'o37, 15'o00002}; //         слиа 1(1)   цикл повтор(1)
    mem[6] = {4'o02, 5'o25, 15'o77777, 4'o02, 5'o34, 15'o00001}; //         слиа -1(2)  пио  начало(2)
    mem[7] = {4'o00, 5'o33, 15'o00000, 4'o00, 5'o30, 15'o00001}; // ошибка: стоп        пб   начало
end

endmodule
