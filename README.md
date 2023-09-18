# hush.el - Fetch and cache _your_ secrets _your_ way.

A convenient and extensible interface for fetching secrets from external password managers.

## Motivation

Most of us use a password manager of sorts to store secrets.
Getting the contents of such secrets while we are in Emacs can get annoying and will cause a context switch.

This library was created to make the process smoother and customizable to anyone's needs.

## Installation
Either download [the library](hush.el) to your `load-path` or use straight/your-favorite-package-manager
```emacs-lisp
(straight-use-package
 '(hush :type git :host github :repo "tirimia/hush"))
```

## Fetching secrets
`hush-get` is the bread and butter of this library.

Use it to fetch your secrets by passing the individual engines their required parameters.

Here are a few examples:
```emacs-lisp
(setq secret-we-put-in-by-hand (hush-get "put secret here" "prompt"))
(setq cool-secret-in-onepassword (hush-get '(:vault "private" "github.com/secretpat") "1password"))
```

With a set `hush-default-engine`, you can just pass the parameters:
```emacs-lisp
(setq hush-default-engine "1password")
(setq cool-secret-in-onepassword (hush-get '(:vault "private" "github.com/secretpat")))

(let ((hush-default-engine "1password"))
    (setq cool-secret-in-onepassword (hush-get '(:vault "private" "github.com/secretpat"))))
```

## Configuration
This library was designed to be extensible.

You can plug in your own caching mechanism by customizing the `hush-cache`.

You can add other secret engines by modifying the `hush-engine-alist`.
