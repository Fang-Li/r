d:
cd D:\test
rem 加载ebin目录所有文件，可以tab关联出来
rem 用start方式启动，可以不等erlang窗口退出的同时退出dos
start werl "-pa" "ebin" -eval "{ok,FileLists} = file:list_dir(ebin),[ begin [File,Suffix] = string:tokens(FileName,\".\") , File2 = erlang:list_to_atom(File), erlang:apply(File2,module_info,[]) end  ||  FileName <- FileLists , FileName =/= \"nif_c.beam\"], make:all([{compile,export_all},debug_info])."

rem 要退出dos，不然挂在那里别扭
exit