" Vim global plugin for working with ROS
" Last Change:  2010 Jul 27
" Maintainer:   Michael Styer <michael@styer.net>

" Helper commands
function! s:RosDecodePath(path)
    let rosname = ""
    let reldir = ""
    let rosdir = ""
    let last = ""

    if match(a:path,'\v.+/.*') == -1
        let rosname = a:path
    else
        let result = matchlist(a:path,'\v^([^/]+)(/.{-})([^/]*)$')
        let rosname = result[1]
        let reldir =  result[2]
        let last =    result[3]
    endif

    let rosdir = s:RosLocationFind(rosname)
    let rosvals = [rosname, rosdir, reldir, last]
    return rosvals
endfunction

function! s:RosLocationFind(name)
    if $ROS_LOCATIONS != ""
        let loc_dict = {}
        for p in split($ROS_LOCATIONS,':')
            let pair = split(p, '=')
            let loc_dict[pair[0]] = pair[1]
        endfor
        if has_key(loc_dict,a:name)
            return loc_dict[a:name]
        endif
    endif

    if a:name == 'log'
        return $ROS_ROOT . "/bin/roslaunch-logs"
    elseif a:name == 'test_results'
        return $ROS_ROOT . "/test/rostest/bin/test-results-dir"
    endif

    let cmd = "export ROS_CACHE_TIMEOUT=-1.0 && rospack find " . a:name . " 2> /dev/null"
    let location = system(cmd)
    if v:shell_error != 0
        let cmd = "export ROS_CACHE_TIMEOUT=-1.0 && rosstack find " . a:name . " 2> /dev/null"
        let location = system(cmd)
    endif
    let location = substitute(location, "\n", "", "")
    return location
endfunction

" Top level commands
function! s:RosChangeDir(...)
    if a:0 == 0 || a:1 == ""
        lcd $ROS_ROOT
        return
    endif

    let rosvals = s:RosDecodePath(a:1)
    if rosvals[1] == ""
        echo "No such package: " . a:1
        return
    else
        let dir = rosvals[1] . rosvals[2] . rosvals[3]
        echo dir
        execute "lcd " . dir
    endif

endfunction

function! Rosed(pack, dir)
    echo "rosed"
endfunction

if !exists(":Roscd")
    command -nargs=? Roscd :call s:RosChangeDir(<f-args>)
endif

