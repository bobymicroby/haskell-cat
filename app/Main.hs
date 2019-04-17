module Main where
import Graphics.Gloss

window :: Display
window = InWindow "BASIC Cat" (280, 160) (0, 0)

ears :: Picture
ears = Line [(50, 20), ( 70,  40), (80,40) ,(100,20), (100,65),(80,80),(70,80), (50,65) , (50,20) ] 

nose :: Picture
nose =    Line [ (70,60), (80,60) , (75,65) , (70,60) ] <>  Line [(75,65) ,(75,75)  ]     

moustaches :: Picture
moustaches = 
    Line [ (40,65), (65,65)  ] <>
    Line [ (85,65), (110,65)  ] <>
    Line [ (40,80), (65,70)  ] <>
    Line [ (85,70), (110,80)  ] <>
    Line [ (50,90), (65,75)  ] <>
    Line [ (85,75), (100,90)  ] 

body :: Picture  
body =  Line [ (100,50),  (120,70),  (190,70), (230,32), (240,45),(190,90), (190,120), (200,130),(200,140),(180,140), (180,130)
    ] <> Line [(180,130), (160,110),(120,110), (120,140), (100,140), (100,100), (80,80)  
    ] <> Line [   (110,110),  (110,140)
    ]
    
eye :: Float -> Float -> Float -> Picture
eye r a b = 
 let
    dots = [0,0.05..2 * 3.1415]
    points = (\fi-> (a+r * (cos fi), b-r *  (sin fi) ) ) <$> dots   
 in  Line points

eyes =  (eye 6 65 50) <> (eye 6 85 50) 
    
eyelids :: Picture  
eyelids =  Line [ 
        (65,45),(65,55)
    ] <> Line [
        (85,45),(85,55)
    ] 

noTail :: Picture  
noTail =  Color green ( Line [ (190,70),(230,32),(240,45),(190,90)] )

tailUp :: Picture  
tailUp =  Line [ (190,70) ,(240,45),(250,65) , (190,90) ]

noEyelids = Color green eyelids

cat :: Picture
cat = ears <> nose <> eyes <> moustaches <> body <> eyelids 

catTailUp :: Picture
catTailUp = cat <> noEyelids <> noTail <> tailUp 

draw :: Float -> Picture
draw seconds = Rotate 180 ( if(even (floor seconds) ) then catTailUp else cat )

main :: IO ()
main = animate window green draw

                