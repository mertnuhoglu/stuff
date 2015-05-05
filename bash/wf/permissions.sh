#!/bin/bash

if [ $# -lt 2 ]; then
    echo 'Too few arguments.\n'
    echo 'USAGE: permissions.sh action username app_name'
    echo 'eg: permissions.sh grant joe django'
    echo 'actions: grant, revoke-app, revoke-all'
    exit
fi

PRIMARY_USER=`whoami`
ACTION=$1
SECONDARY_USER=$2
APP_DIR=$3

if [ $ACTION == "grant" -a $# == 3 ]; then
    echo 'Granting permissions to' $APP_DIR 'for user' $SECONDARY_USER'.'
    if [ ! `getfacl -p $HOME | grep user:$SECONDARY_USER:--x` ]; then
        # Grant secondary user access to the primary account's home directory
        # for navigational purposes only.
        setfacl -m u:$SECONDARY_USER:--x $HOME
        # Disallow ALL access to all direcories for secondary user.                                
        setfacl -R -m u:$SECONDARY_USER:--- $HOME/webapps/*                                        
    fi                                                                                             
    # Grant secondary user permissions to specified app directory.                                 
    setfacl -R -m u:$SECONDARY_USER:rwx $HOME/webapps/$APP_DIR                                     
    # Grant secondary user permissions to new directories created                                  
    # in specified app directory.                                                                  
    setfacl -R -m d:u:$SECONDARY_USER:rwx $HOME/webapps/$APP_DIR                                   
    # Ensure all new directories and files are owned by the primary                                
    # account's group.                                                                             
    chmod g+s $HOME/webapps/$APP_DIR                                                               
    # Make sure the primary account user continues to have full access                             
    # to all files and directories.                                                                
    setfacl -R -m d:u:$PRIMARY_USER:rwx $HOME/webapps/$APP_DIR

elif [ $ACTION == "revoke-app" ]; then                                                             
    echo 'Revoking permissions for user' $SECONDARY_USER 'to' $APP_DIR'.'
    setfacl -R -x u:$SECONDARY_USER $HOME/webapps/$APP_DIR
    setfacl -R -x d:u:$SECONDARY_USER $HOME/webapps/$APP_DIR

elif [ $ACTION == "revoke-all" ]; then
    echo 'Revoking ALL permissions for' $SECONDARY_USER'.'
    setfacl -x u:$SECONDARY_USER $HOME
    setfacl -R -x u:$SECONDARY_USER $HOME/webapps/*
    setfacl -R -x d:u:$SECONDARY_USER $HOME/webapps/*
fi
