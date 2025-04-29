[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/dPwN1w3S)
# Final project

**Explain the general theme and specific features of your project.**

I tried to prove that there are infinitely many prime numbers. The general outline I followed was Euclid's proof. I made a function to make a list of primes, a function to find the product of that list plus one, a function to show that there would always be one prime not included in the list that was unique from the primes in the list. The project utilized record types of Prime, Proper Divisor, Divisor, and Prime Divisor from our previous project. Additionally, it uses Vector data type, All data type, Distinct data type, and an Element data type. The Vector data type represents the list. The All data type is used when definining a certain property of all elements in a list. The Distinct Data type is used to define an element that is distinct from other elements in a list. The Element Data type is used to define an element of a list. 
**Cite any resources or existing code you used.**
I used ChatGPT to give me the idea to solve the hole for my prime-distinct function. It suggested making a helper function and helped me define the type for the helper function. I also used ChatGPT to correctly define the base case for my "product" function which kept giving me an error. It explained to me that the product of the elements in an empty list has to be 1 as the base case because if it were 0, then when it calls it recursively, the product of any list of length greater than 1 will also be zero. 


**Discuss any challenges, or anything you'd like feedback on.**
I struggled to fill in the helper functions (primes-valid-helper and new-primes-not-in) so I would like some feedback on that. Specifically, the goal type for the holes is contradiction and I wasn't sure how to fill this in. I also would like some help filling in the first hole in primes-valid. Everytime I C-c C-comma, it kept giving me the same three step sequence of prim ( 1<new-primes(...)(...)(primes-valid-helper ...) and I wasn't sure how to fix this. I was thinking of making a helper function, but I wasn't sure how to define it. I would also like feedback on my definition for new-prime. I want to know if there is a way to write it without using the lambda x x1. Finally, I would like some feedback on (prop-div nat div greater less) in prime-valid-helper. I'm not sure what this means, but I C-c C-c and agda filled this in for me.


