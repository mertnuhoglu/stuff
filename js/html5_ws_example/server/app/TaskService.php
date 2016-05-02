<?php
/**
 * Created by IntelliJ IDEA.
 * User: remi
 * Date: 14/03/15
 * Time: 16:38
 */

namespace Todo;


use Symfony\Component\HttpFoundation\File\Exception\FileNotFoundException;

class TaskService {

    private $file = null;
    private $tasks = array();
    public function __construct($file = ""){
        if( ! is_file($file) ){
            throw new FileNotFoundException($file);
        }
        $this->file = $file;
        $this->tasks = json_decode(file_get_contents($this->file), true);

    }


    public function allTasks()
    {
        return $this->tasks;
    }

    public function addTask(Task $task)
    {
        array_push($this->tasks, $task->__toArray());
        $this->save();
    }

    private function save()
    {
        file_put_contents($this->file, json_encode($this->tasks));
    }

    public function removeTask(Task $task)
    {
        $tmp = array();
        $found = false;
        foreach($this->tasks as $taskArray){
            if( $taskArray['id'] == $task->getId()){
                $found = true;
            } else{
                array_push($tmp, $taskArray);
            }
        }
        if( $found ){
            $this->tasks = $tmp;
            $this->save();
        }

        return $found;
    }

    public function updateTask( Task $task)
    {
        $tmp = array();
        $found = false;
        foreach($this->tasks as $taskArray){
            if( $taskArray['id'] == $task->getId()){
                $found = true;
                array_push($tmp, $task->__toArray());
            } else{
                array_push($tmp, $taskArray);
            }
        }
        if( $found ){
            $this->tasks = $tmp;
            $this->save();
        }

        return $found;
    }

    public function clear()
    {
        $this->tasks = [];
        $this->save();
    }

}