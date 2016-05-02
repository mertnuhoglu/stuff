<?php
/**
 * Created by IntelliJ IDEA.
 * User: remi
 * Date: 14/03/15
 * Time: 16:53
 */

namespace Todo;


class TaskMessage {


    const LIST_ACTION = 0;
    const ADD_ACTION = 1;
    const DELETE_ACTION = 2;
    const EDIT_ACTION = 3;
    const CLEAR_ACTION = 4;

    private $message;
    private $action;

    function __construct($action = TaskMessage::LIST_ACTION)
    {
        $this->action = $action;
    }


    /**
     * The message to send, i will apply json_encode on it.
     * @param $message
     */
    public function content($message)
    {
        if( $message instanceof Task ){
            $message = $message->__toArray();
        }
        $this->message = $message;
    }

    public function send()
    {
        $send  = array(
            'action' => $this->action,
            'message'=>$this->message
        );
        return json_encode($send);
    }
}