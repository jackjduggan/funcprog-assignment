1. **Student Name:** Jack Duggan

2. **Main Learnings of Project**
- I found the use of the `Either` type to handle errors (`Left`, `Right`) to be a very interesting way of doing such error handling.
- Seeing techniques of parsing CSV data into objects that are well structured.
- Using vectors to handle large data, in this case a bunch of Module descriptors.
- Generating markdown programatically.. I had never done something like this on such a scale.
- Found the Stack error messages to often be helpful when it came to solving errors, with it even regularly suggesting fixes.
- Suggested plan of action was followed as best as possible.
- Brief project description is as follows: A CSV file is read and parsed into `Module` objects. Each module is validated field-by-field, producing a `ValidatedModule` that either contains valid data or the appropriate error message explaining the invalidity. Two markdown files are then generated, one with all the modules (including those with errors), and one with only the fully validated modules.

3. **Main Difficulties That You Came Across**
- At first I was having difficulty grasping the point of the Either type, but once it clicked it made good sense.
- A couple of aspects required some trial and error to get working, generally related to syntax.
- Tried following the suggested plan of action regarding the generation of 2 CSV files (valid/invalid) from which to generate markdown files. I spent a while at this, using toNamedRecord and other, but ultimately scrapped it. 
- ValidateIndicativeContent required a check that *each sentence should start with a capital letter*. I struggled to figure out a way to divide a string into sentences, with Google being no help. Eventually though about doing some of splitting using a full stop as a delimeter, since every sentence should have a full stop. Used a helper function *splitIntoSentences* to split the string into sentences based on occurrence of a full stop/period, with help from a StackExchange article. The *customGroup* helper function then determines how to group characters into these sentences, with False returning if the current character is a full stop, breaking the grouping and marking the end of the sentences.
- When writing the *isFullyValidated* function, I encountered an error within the *isRight* check where the type from validatedCredits *int* was not matching expected *Either String String*. After a bit of hardship trying to resolve this, I added a second isRight 'check' afterwards that handled the validatedCredits separately. This appeared to work.
- Writing the validations was generally difficult, and I didn't end up implementing all of them. I feel some of them may not be implemented correctly as I didn't thoroughly test them.

4. **Any extra functionalities not mentioned in the spec that you implemented**
- The suggested plan of action mentioned creating two CSV files and generating markdown files from them. As mentioned above I had problems with the CSV generation, and as such generated the markdown directly.
- As well as above, the two collections of validated modules weren't created explicitly, but rather 'on-the-fly' in the markdown generation step.

5. **Reference to any material outside of notes used**
- ref "By doing Shape(..), we exported all the value constructors for Shape" https://learnyouahaskell.com/making-our-own-types-and-typeclasses
- ref https://hackage.haskell.org/package/Cassava-0.5.1.0/docs/Data-Csv.html
- ref "This code with groupBy from Data.List..." https://codereview.stackexchange.com/questions/6992/approach-to-string-split-by-character-in-haskell
- ref https://hackage.haskell.org/package/base-4.19.1.0/docs/Prelude.html#v:lines
- ref https://chat.openai.com/share/85470ab5-4e35-4983-8130-f2f81497f5e5