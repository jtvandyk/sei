SELECT "sei"."master"."GID",
"sei"."master"."dStateFP",
"sei"."master"."dNCESID",
"sei"."master"."dName",
"sei"."master"."dLoGrade",
"sei"."master"."dHiGrade",
"sei"."master"."dStatePost",
"sei"."master"."dEstPop",
"sei"."master"."dEstStud",
"sei"."master"."dEstPov",
"sei"."master"."dEstPovRate",
"sei"."master"."year",
"sei"."master"."geom"
--Type in the two character state code for the creation of the relevant table
INTO "sei"."<dStatePost>"
FROM "sei"."master"
--Copy the state character code used in above here
WHERE "sei"."master"."dStateFP" = '<dStateFP>'
ORDER BY "sei"."master"."dName"