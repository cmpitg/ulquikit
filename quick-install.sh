#!/bin/sh

if [ `which racket >/dev/null 2>&1 && echo t || echo f` == "f" ]; then
    echo "-> Racket not found, please install it first."
    echo "   You might refer to your OS's package manager to install Racket,"
    echo "   or download it from: http://racket-lang.org/download/"
    echo "   Please MAKE SURE you have Racket 6+."
    echo "-> Installation aborted."
    exit 1
else
    echo "-> Found Racket.  MAKE SURE you have Racket 6+."
fi


if [ `which ruby >/dev/null 2>&1 && echo t || echo f` == "f" ]; then
    echo "-> Ruby not found."
    echo "   You might refer to your OS's package manager to install Ruby."
    echo "   However, this script could install Ruby for you using RVM stable."
    echo "   Please refer to http://rvm.io for further information."

    echo -n "-> Would you like to install RVM stable single-user mode? [Y/n] "
    read DO_INSTALL_RVM

    if [ "$DO_INSTALL_RVM" == "" ] \
        || [ "$DO_INSTALL_RVM" == "y" ] \
        || [ "$DO_INSTALL_RVM" == "Y" ]; then
        echo "-> Installing Ruby 1.9 and RVM..."

        \curl -sSL https://get.rvm.io | bash -s stable
        [[ -f ~/.bashrc ]] && (echo 'source $HOME/.rvm/scripts/rvm' >> ~/.bashrc)
        [[ -f ~/.zshrc  ]] && (echo 'source $HOME/.rvm/scripts/rvm' >> ~/.zshrc)
        source $HOME/.rvm/scripts/rvm
        rvm install 1.9
        rvm use 1.9 --default
    else
        echo "-> Installation aborted."
        exit 1
    fi
else
    echo "-> Found Ruby.  MAKE SURE you have Ruby 1.9+."
fi

if [ `which asciidoctor >/dev/null 2>&1 && echo t || echo f` == "t" ]; then
    echo "-> AsciiDoctor found."
else
    echo "-> Installing AsciiDoctor..."
    gem install -V asciidoctor
fi


if [ `(raco pkg show | grep rackjure) >/dev/null 2>&1 && echo t || echo f` == "t" ]; then
    echo "-> Rackjure found."
else
    echo "-> Installing Rackjure..."
    raco pkg install rackjure
fi


DOWNLOAD_URL=https://github.com/cmpitg/ulquikit/releases/download/v0.1/ulquikit-0.1.zip

echo -n "-> Where would you like to install/update Ulquikit? (default: $HOME/) "
read ULQUIKIT_DEST
eval ULQUIKIT_DEST=$ULQUIKIT_DEST

if [ "$ULQUIKIT_DEST" == "" ]; then
    ULQUIKIT_DEST=$HOME/
fi

cd $ULQUIKIT_DEST

echo "-> Downloading latest version..."
wget -q "$DOWNLOAD_URL" -O ulquikit.zip

echo "-> Unpacking..."
unzip ulquikit.zip

echo "-> Removing zip file..."
rm -f ulquikit.zip

if [ `which ulqui >/dev/null 2>&1 && echo t || echo f` == "f" ]; then
    echo '-> Adding ulquikit/bin to your $PATH'
    [[ -f ~/.bashrc ]] && (echo export PATH=$ULQUI_DIR/release/ulquikit/bin:'$PATH' >> ~/.bashrc)
    [[ -f ~/.zshrc ]] && (echo export PATH=$ULQUI_DIR/release/ulquikit/bin:'$PATH' >> ~/.zshrc)

    echo "-> Done!  Enjoy your time with literate programming!"
else
    echo '-> Found ulqui command in your $PATH.'
fi

