# For Translaters

This directory contains translation files for each language.

Note: This document is not specific to this project.  
URLs and program names that appear in the middle are fictional, so please read them as appropriate.

## Tools needed for update work

Translation work can be done without a program compilation environment.

- Git for Windows  
  https://gitforwindows.org/  
  In practice, you can work on Linux if you have tools like `bash`, `curl`, and `gettext`.
- Poedit  
  https://poedit.net/  
  It's not necessary, but it will make your work easier.

## Files handled in update work

In the translation file update work, the following files are used.  
**Only `*.po` should be edited manually by humans**.

- `*.pot` files
  - These are template files that extract the parts to be translated from the source code.
  - Since this file can be automatically generated, it is not managed in the repository.
  - This is not a file for humans to edit directly.
- `*.po.DO_NOT_EDIT` files
  - These are files for each language that hold the current translation data.
  - This file is managed in the repository.
  - This is not a file for humans to edit directly.
- `*.po` files
  - These are files for humans to do translation work.
  - This file is generated from `*.pot` and `*.po.DO_NOT_EDIT`.
  - After editing, `*.po.DO_NOT_EDIT` is updated based on this file.
- `*.mo` files
  - These are compiled from `*.po.DO_NOT_EDIT` and are files to be integrated into the program.
  - Since this file can be automatically generated, it is not managed in the repository.
  - This is not a file for humans to edit directly.

## Workflow of update work

The update work is done in the following flow.  
Although there are many steps, multiple steps will be completed with a single command.

1. Clone the repository
    - This is necessary to do the work.
2. Generate the `*.pot` file
    - This is necessary to create a translation file that corresponds to the current source code.
3. Generate the `*.po` file
    - Create a working file based on the prepared `*.pot` and `*.po.DO_NOT_EDIT`.
    - If there is no `*.po.DO_NOT_EDIT`, a new `*.po` will be created.
4. Edit the `*.po` file
    - You can edit it with just a text editor, or you can use [Poedit](https://poedit.net/).
5. Generate a new `*.po.DO_NOT_EDIT` file
    - Generate a `*.po.DO_NOT_EDIT` based on the edited `*.po` file.
6. Generate the `*.mo` file
    - Generate the `*.mo` using the `*.po.DO_NOT_EDIT`.
7. Integrate the `*.mo` into the program
    - Perform a function test by actually integrating the `*.mo`.
8. Commit the `*.po.DO_NOT_EDIT` to the repository
    - This completes the update work.
    - It would be appreciated if you could submit a pull request with the changes.

## 1. Clone the repository

To do the work, clone the project repository.  
If you're going to submit a pull request in the end, it's a good idea to fork and use your own repository.

```bash
git clone https://github.com/user/project
```

## 2. Generate the `*.pot` file / 3. Generate the `*.po` file

Steps 2 and 3 are automatically completed by running a script.  
Open this directory in Git BASH and run the following command:

```bash
bash start.bash
```

This will create translation work files corresponding to the current source code.  
However, if `*.po` already exists, the file will not be generated.  
If you have work-in-progress files, delete them all and rerun the script.

## 4. Edit the `*.po` file

This is a GNU gettext `*.po` file.  
Proceed with translation work as usual.

You can edit it with just a text editor, or you can use [Poedit](https://poedit.net/).

## 5. Generate a new `*.po.DO_NOT_EDIT` file / 6. Generate the `*.mo` file / 7. Integrate the `*.mo` into the program

Steps 5, 6, and 7 are automatically completed by running a script.

After editing, open this directory in Git BASH and run the following command:

```bash
bash finish.bash
```

This will generate an updated `*.po.DO_NOT_EDIT` and `*.mo` all at once.

If you want to integrate the translation data into a compiled program, pass the path to the executable file.

```bash
# If program.exe is in the same folder
bash finish.bash program.exe
```

This will write the new translation data into `program.exe`.

Note that the embedding of translation data uses a standard Windows mechanism.  
You can also apply changes by replacing them with tools like [Resource Hacker](http://www.angusj.com/resourcehacker/), but to make this operation easy from the command line, a simple open-source program called [minirh](https://github.com/oov/minirh) was created.  
`finish.bash` uses minirh, and if minirh does not yet exist, it will be automatically downloaded, placed in the `.tool` directory, and used.

## 8. Commit the `*.po.DO_NOT_EDIT` to the repository

If you want to commit your work to the repository, commit the `*.po.DO_NOT_EDIT`.  
This file has been processed to minimize changes.
