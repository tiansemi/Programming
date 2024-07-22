# KeePass Entries Diff Checker

## Description

This Python script compares two KeePass (kdbx) files and highlights the differences between the entries. It identifies entries that are unique to each file, entries that are identical in both files, and entries that have different passwords. Additionally, it displays the URLs associated with each entry.

## Features

- **Compare two KeePass files**: Identify entries unique to each file and entries with matching or differing passwords.
- **Highlight differences**: Display differences with color coding for easy identification.
- **Display URLs**: Show URLs associated with each entry for better context.

## Requirements

- `Python 3.x`
- `pykeepass` library for providing the Python interface to KeePass databases.
- `termcolor` library for enabling colored terminal text.

## Installation

```sh
curl -O https://raw.githubusercontent.com/tiansemi/Programming/main/Python/KeePass/compare_kdbx.py && chmod +x compare_kdbx.py
```

## Full Installation

```sh
curl -O https://raw.githubusercontent.com/tiansemi/Programming/main/Python/KeePass/install_compare_kdbx.py && chmod +x install_compare_kdbx.sh && sudo ./install_compare_kdbx.sh
```

## Full Uninstallation

```sh
curl -O https://raw.githubusercontent.com/tiansemi/Programming/main/Python/KeePass/uninstall_compare_kdbx.py && chmod +x uninstall_compare_kdbx.sh && sudo ./uninstall_compare_kdbx.sh
```


## Usage

```sh
python3 compare_kdbx.py <path/to/file1.kdbx> <path/to/file2.kdbx> <password1> <password2>

