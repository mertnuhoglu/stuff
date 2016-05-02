<?php
namespace Todo;

class Task
{

    private $id;

    private $task;

    function __construct($id = null, $task = null)
    {
        $this->id = $id;
        $this->task = $task;
    }


    public function __toArray(){
        return array(
            'id' => $this->id,
            'task' => $this->task
        );
    }

    function __toString()
    {
        return json_encode($this->__toArray());
    }

    /**
     * @return mixed
     */
    public function getId()
    {
        return $this->id;
    }

    /**
     * @param mixed $id
     */
    public function setId($id)
    {
        $this->id = $id;
    }

    /**
     * @return mixed
     */
    public function getTask()
    {
        return $this->task;
    }

    /**
     * @param mixed $message
     */
    public function setTask($message)
    {
        $this->task = $message;
    }



}