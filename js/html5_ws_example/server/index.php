<?php
/**
 * Created by IntelliJ IDEA.
 * User: remi
 * Date: 07/03/15
 * Time: 10:24
 */

use Ratchet\Http\HttpServer;
use Ratchet\Server\IoServer;
use Ratchet\WebSocket\WsServer;
use Todo\TaskWs;
use Todo\TaskService;


require dirname(__FILE__) . '/vendor/autoload.php';
require dirname(__FILE__) . '/app/TaskWs.php';
require dirname(__FILE__) . '/app/TaskMessage.php';
require dirname(__FILE__) . '/app/TaskService.php';
require dirname(__FILE__) . '/app/Task.php';

$data = realpath( dirname(__FILE__) . '/datas/tasks.json');

$server = IoServer::factory(new HttpServer(new WsServer(new TaskWs(new TaskService($data)))), 8080);

$server->run();