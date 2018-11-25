set /P msg=Enter Message: 
git add *
git commit -m "%msg%"
git push
cd ..
git add commonx_demos
git commit -m "%msg%"
git push
cd commonx_demos
pause

