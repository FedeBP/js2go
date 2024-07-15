function fibonacci(n) {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

function printFibonacciSequence(count) {
    for (let i = 0; i < count; i++) {
        console.log(fibonacci(i));
    }
}

let maxNumber = 10;
let sum = 0;

for (let i = 1; i <= maxNumber; i++) {
    if (i % 2 === 0) {
        sum += i;
    }
}

console.log("Sum of even numbers up to " + maxNumber + " is: " + sum);

printFibonacciSequence(8);