<?php

namespace Todo;

use Ratchet\MessageComponentInterface;
use Ratchet\ConnectionInterface;

class TaskWs implements MessageComponentInterface
{

    /**
     * The Client list
     * @var \SplObjectStorage
     */
    protected $clients;


    /**
     * @var TaskService
     */
    protected $service;


    /**
     * Initialise the service
     */
    public function __construct(TaskService $service)
    {
        $this->clients = new \SplObjectStorage;
        $this->service = $service;
    }


    /**
     * User Connect to the service
     * @param ConnectionInterface $conn
     */
    public function onOpen(ConnectionInterface $conn)
    {
        // we attach (connect, add) the client to the client list.
        $this->clients->attach($conn);

        // We need to send the Task list to the client
        $message = new TaskMessage(TaskMessage::LIST_ACTION);
        $message->content($this->service->allTasks());
        $conn->send($message->send());
    }

    /**
     * The Main program, the message receiver from client.
     *
     * @param ConnectionInterface $from
     * @param string $msgReceived
     * @internal param string $msg
     */
    public function onMessage(ConnectionInterface $from, $msgReceived)
    {


        // First we need to parse the message received:
        $received = json_decode($msgReceived, true);

        switch ($received["action"]) {
            case TaskMessage::ADD_ACTION:
                // We need to create the task :
                $task = new Task($received['data']['id'], $received['data']['task']);
                // and add it to the persitence
                $this->service->addTask($task);
                // then send the task to other clients
                $message = new TaskMessage(TaskMessage::ADD_ACTION);
                $message->content($task);
                $this->sendToOthers($from, $message);
                break;
            case TaskMessage::DELETE_ACTION:
                $task = new Task($received['data']['id']);
                $this->service->removeTask($task);
                $message = new TaskMessage(TaskMessage::DELETE_ACTION);
                $message->content($task);
                $this->sendToOthers($from, $message);
                break;
            case TaskMessage::EDIT_ACTION:
                $task = new Task($received['data']['id'], $received['data']['task']);
                $this->service->updateTask($task);
                $message = new TaskMessage(TaskMessage::EDIT_ACTION);
                $message->content($task);
                $this->sendToOthers($from, $message);
                break;
            case TaskMessage::CLEAR_ACTION:
                $this->service->clear();
                $message = new TaskMessage(TaskMessage::CLEAR_ACTION);
                $message->content('');
                $this->sendToOthers($from, $message);
                break;
            default:
        }


    }


    public function sendToOthers(ConnectionInterface $from, TaskMessage $message)
    {
        foreach ($this->clients as $client) {
            if ($from !== $client) {
                // The sender is not the receiver, send to each client connected
                $client->send($message->send());
            }
        }
    }


    /**
     * Connection is closed by the user.
     *
     * @param ConnectionInterface $conn
     */
    public function onClose(ConnectionInterface $conn)
    {
        // We detach (disconnect, remove) the client from the client list
        $this->clients->detach($conn);
    }


    /**
     * Catch all connection errors
     *
     * @param ConnectionInterface $conn
     * @param \Exception $e
     */
    public function onError(ConnectionInterface $conn, \Exception $e)
    {
        echo "An error has occurred: {$e->getMessage()}\n";
        $conn->close();
    }
}