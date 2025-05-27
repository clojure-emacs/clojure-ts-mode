(ns embed)

(js* "var hello = console.log('hello'); const now = new Date();")

(js* "const hello = new Date();
      const someOtherVar = 'Just a string';")

(println "This is a normal string")

"Standalone string"

(js* "var hello = 'world';")
