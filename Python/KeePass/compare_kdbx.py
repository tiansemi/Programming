import sys
from pykeepass import PyKeePass
from termcolor import colored

def read_kdbx(file, password):
    kp = PyKeePass(file, password=password)
    entries = {entry.title: (entry.password, entry.url) for entry in kp.entries}
    return entries

def diff_kdbx(entries1, entries2):
    diff = {
        'only_in_first': {},
        'only_in_second': {},
        'different': {},
        'same': {}
    }
    
    all_titles = set(entries1.keys()).union(set(entries2.keys()))
    
    for title in all_titles:
        if title in entries1 and title in entries2:
            if entries1[title][0] == entries2[title][0]:
                diff['same'][title] = entries1[title]
            else:
                diff['different'][title] = (entries1[title], entries2[title])
        elif title in entries1:
            diff['only_in_first'][title] = entries1[title]
        else:
            diff['only_in_second'][title] = entries2[title]
    
    return diff

def print_diff(diff):
    print(colored('Entries only in the first file:', 'yellow'))
    if diff['only_in_first']:
        for title, (password, url) in diff['only_in_first'].items():
            print(f"Title: {title}, Password: {colored(password, 'yellow')}, URL: {url}")
    else:
        print(colored('none', 'red'))
    
    print(colored('\nEntries only in the second file:', 'blue'))
    if diff['only_in_second']:
        for title, (password, url) in diff['only_in_second'].items():
            print(f"Title: {title}, Password: {colored(password, 'blue')}, URL: {url}")
    else:
        print(colored('none', 'red'))
    
    print(colored('\nEntries with different passwords:', 'magenta'))
    if diff['different']:
        for title, ((password1, url1), (password2, url2)) in diff['different'].items():
            print(f"Title: {title}, Passwords: {colored(password1, 'magenta')} | {colored(password2, 'magenta')}, URLs: {colored(url1, 'magenta')} | {colored(url2, 'magenta')}")
    else:
        print(colored('none', 'red'))
    
    print(colored('\nEntries with the same passwords:', 'green'))
    if diff['same']:
        for title, (password, url) in diff['same'].items():
            print(f"Title: {title}, Password: {colored(password, 'green')}, URL: {colored(url, 'green')}")
    else:
        print(colored('none', 'red'))

if __name__ == '__main__':
    if len(sys.argv) != 5:
        print("Usage: python compare_kdbx.py <file1.kdbx> <file2.kdbx> <password1> <password2>")
        sys.exit(1)
    
    file1, file2, password1, password2 = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4]
    
    entries1 = read_kdbx(file1, password1)
    entries2 = read_kdbx(file2, password2)
    
    diff = diff_kdbx(entries1, entries2)
    
    print_diff(diff)
