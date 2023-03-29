# QIP Instructions

Execute the main.R file to run the package.

Any configuration changes should be made in the config.yml file where you can
change the input and output locations, select columns, and specify keywords to
match themes or support owners.

## Info on keywords

- Not case sensitive
- Specific about symbols e.g. "&" vs "and"
- To find an exact word match use "//binsert_text_here//b"
- To add an optional ending use "?"

e.g. "//bRAPs?//b" would find "RAP", "rap", "RAPS", "RAPs", etc.
but not "trap", "traps", or "rappers"
