#include <stdio.h>
#include <stdlib.h>

// Include GLEW
#include <GL/glew.h>

// Include GLFW
#include <GLFW/glfw3.h>
GLFWwindow* window;

// Include GLM
#include <glm/glm.hpp>
using namespace glm;

const char *vertexShaderSource = 
R"(#version 330 core
layout (location = 0) in vec2 position;            
layout (location = 1) in vec2 inTexCoord;

out vec2 texCoord;
void main(){
    texCoord = inTexCoord;
    gl_Position = vec4(position.x, position.y, 0.0f, 1.0f);
})";

const char *fragmentShaderSource2 =
R"(#version 330 core
precision lowp float;
out vec4 fragColor;
vec2 fragCoord = gl_FragCoord.xy;
uniform vec2 iMouse;
uniform vec2 iResolution;
uniform float iTime;
#define t iTime
#define bpm 60.
#define B (60./bpm)
#define b (mod(t,B)/B)

#define f(p)if(p.x<p.y)p=p.yx;

vec3 rot(vec3 p,vec3 a,float t){
  a=normalize(a);
  return mix(a*dot(p,a),p,cos(t))+sin(t)*cross(p,a);
}

void main()
{
    vec2 uv = (2.* gl_FragCoord.xy - iResolution.xy )/iResolution.y;
    vec3 p,c=vec3(0),d=normalize(vec3(uv,2));
    float g=0.,e,s,a;
    for(float i=0.;i<99.;i++){
        p=g*d;
        p.z-=7.;
        p=rot(p,vec3(1,1,1),mix(.5,1.2,mod(t,4.)/4.));
        s=2.;
        p=abs(p)-1.8;
        f(p.xy)
        f(p.xz)
        for(int i=0;i<10;i++){
            p=.8-abs(p-1.2);
            f(p.xz)
            s*=e=3./clamp(dot(p,p),.0,1.2);
            p=abs(p)*e-vec3(.3,9.+b*b*5.,6);
        }
        a=1.;
        p-=clamp(p,-a,a);
        g+=e=length(p)/s;
        e<.001?c+=pow(cos(i/64.),5.)*.02
            *mix(vec3(1),(cos(vec3(1,2,3)+log(s)+t)*.5+.5),.5)
            :p;
    }
    fragColor = vec4(c,1.0);
})"; 


const char *fragmentShaderSource = 
R"(#version 330 core
in vec2 texCoord;
uniform vec2 iMouse;
uniform vec2 iResolution;
uniform float iTime;
out vec4 fragColor;
vec2 fragCoord = gl_FragCoord.xy;
#define PI 3.14159265359
#define TWOPI 6.28318530718
//drag the window LR to control roughness

//--graphics setting (lower = better fps)---------------------------------------------------------------------
#define AVERAGECOUNT 16
#define MAX_BOUNCE 32

//--scene data---------------------------------------------------------------------
#define SPHERECOUNT 6
//xyz = pos, w = radius
const vec4 AllSpheres[SPHERECOUNT]=vec4[SPHERECOUNT](
    vec4(0.0,0.0,0.0,2.0),//sphere A
    vec4(0.0,0.0,-1.0,2.0),//sphere B
    vec4(0.0,-1002.0,0.0,1000.0),//ground
    vec4(0.0,0.0,+1002,1000.0),//back wall
    vec4(-1004.0,0.0,0.0,1000.0),//left wall    
    vec4(+1004.0,0.0,0.0,1000.0)//right wall
);
//-----------------------------------------------------------------------
float raySphereIntersect(vec3 r0, vec3 rd, vec3 s0, float sr) {
    // - r0: ray origin
    // - rd: normalized ray direction
    // - s0: sphere center
    // - sr: sphere radius
    // - Returns distance from r0 to first intersecion with sphere,
    //   or -1.0 if no intersection.
    float a = dot(rd, rd);
    vec3 s0_r0 = r0 - s0;
    float b = 2.0 * dot(rd, s0_r0);
    float c = dot(s0_r0, s0_r0) - (sr * sr);
    if (b*b - 4.0*a*c < 0.0) {
        return -1.0;
    }
    return (-b - sqrt((b*b) - 4.0*a*c))/(2.0*a);
}
//-----------------------------------------------------------------------
struct HitData
{
    float rayLength;
    vec3 normal;
};
HitData AllObjectsRayTest(vec3 rayPos, vec3 rayDir)
{
    HitData hitData;
    hitData.rayLength = 9999.0; //default value if can't hit anything

    for(int i = 0; i < SPHERECOUNT; i++)
    {
        vec3 sphereCenter = AllSpheres[i].xyz;
        float sphereRadius = AllSpheres[i].w;
        //----hardcode sphere pos animations-------------------------------------
        if(i == 0)
        {
            float t = fract(iTime * 0.7);
            t = -4.0 * t * t + 4.0 * t;
            sphereCenter.y += t * 0.7;
            
            sphereCenter.x += sin(iTime) * 2.0;
            sphereCenter.z += cos(iTime) * 2.0;
        }
             
        if(i == 1)
        {
            float t = fract(iTime*0.47);
            t = -4.0 * t * t + 4.0 * t;
            sphereCenter.y += t * 1.7;
            
            sphereCenter.x += sin(iTime+3.14) * 2.0;
            sphereCenter.z += cos(iTime+3.14) * 2.0;
        }             
        //---------------------------------------
                
        float resultRayLength = raySphereIntersect(rayPos,rayDir,sphereCenter,sphereRadius);
        if(resultRayLength < hitData.rayLength && resultRayLength > 0.001)
        {
            //if a shorter(better) hit ray found, update
            hitData.rayLength = resultRayLength;
            vec3 hitPos = rayPos + rayDir * resultRayLength;
        hitData.normal = normalize(hitPos - sphereCenter);
        }
    }
    
    //all test finished, return shortest(best) hit data
    return hitData;
}
//--random functions-------------------------------------------------------------------
float rand01(float seed) { return fract(sin(seed)*43758.5453123); }
vec3 randomInsideUnitSphere(vec3 rayDir,vec3 rayPos, float extraSeed)
{
    return vec3(rand01(iTime * (rayDir.x + rayPos.x + 0.357) * extraSeed),
                rand01(iTime * (rayDir.y + rayPos.y + 16.35647) *extraSeed),
                rand01(iTime * (rayDir.z + rayPos.z + 425.357) * extraSeed));
}
//---------------------------------------------------------------------
vec4 calculateFinalColor(vec3 cameraPos, vec3 cameraRayDir, float AAIndex)
{
    //init
    vec3 finalColor = vec3(0.0);
    float absorbMul = 1.0;
    vec3 rayStartPos = cameraPos;
    vec3 rayDir = cameraRayDir;
    
    //only for CineShader, to show depth
    float firstHitRayLength = -1.0;
    
    //can't write recursive function in GLSL, so write it in a for loop
    //will loop until hitting any light source / bounces too many times
    for(int i = 0; i < MAX_BOUNCE; i++)
    {
        HitData h = AllObjectsRayTest(rayStartPos + rayDir * 0.0001,rayDir);//+0.0001 to prevent ray already hit at start pos
        
        //only for CineShader, to show depth
        firstHitRayLength = firstHitRayLength < 0.0 ? h.rayLength : firstHitRayLength;
               
        //if ray can't hit anything, rayLength will remain default value 9999.0
        //which enters this if()
        //** 99999 is too large for mobile, use 9900 as threshold now **
        if(h.rayLength >= 9900.0)
        {
            vec3 skyColor = vec3(0.7,0.85,1.0);//hit nothing = hit sky color
            finalColor = skyColor * absorbMul;
            break;
        }   
               
    absorbMul *= 0.8; //every bounce absorb some light(more bounces = darker)
        
        //update rayStartPos for next bounce
    rayStartPos = rayStartPos + rayDir * h.rayLength; 
        //update rayDir for next bounce
        float rougness = 0.05 + iMouse.x / iResolution.x; //hardcode "drag the window LR to control roughness"
    rayDir = normalize(reflect(rayDir,h.normal) + randomInsideUnitSphere(rayDir,rayStartPos,AAIndex) * rougness);       
    }
    
    return vec4(finalColor,firstHitRayLength);//alpha nly for CineShader, to show depth
}
//-----------------------------------------------------------------------
void main() {
    // Normalized pixel coordinates (from 0 to 1)
    vec2 uv = fragCoord/iResolution.xy;
    
  uv = uv * 2.0 - 1.0;//transform from [0,1] to [-1,1]
    uv.x *= iResolution.x / iResolution.y; //aspect fix

    vec3 cameraPos = vec3(sin(iTime * 0.47) * 4.0,sin(iTime * 0.7)*8.0+6.0,-25.0);//camera pos animation
    vec3 cameraFocusPoint = vec3(0,0.0 + sin(iTime),0);//camera look target point animation
    vec3 cameraDir = normalize(cameraFocusPoint - cameraPos);
    
    //TEMPCODE: fov & all ray init dir, it is wrong!!!!
    //----------------------------------------------------
    float fovTempMul = 0.2 + sin(iTime * 0.4) * 0.05;//fov animation
    vec3 rayDir = normalize(cameraDir + vec3(uv,0) * fovTempMul);
    //----------------------------------------------------

    vec4 finalColor = vec4(0);
    for(int i = 1; i <= AVERAGECOUNT; i++)
    {
        finalColor+= calculateFinalColor(cameraPos,rayDir, float(i));
    }
    finalColor = finalColor/float(AVERAGECOUNT);//brute force AA & denoise
    finalColor.rgb = pow(finalColor.rgb,vec3(1.0/2.2));//gamma correction
    
    //only for CineShader, to show depth
    float z = finalColor.w; //z is linear world space distance from camera to surface
    float cineShaderZ; //expect 0~1
    cineShaderZ = pow(clamp(1.0 - max(0.0,z-21.0) * (1.0/6.0),0.0,1.0),2.0);
    
    //result
    fragColor = vec4(finalColor.rgb,cineShaderZ);
})";

const char *fragmentShaderSource3 = 
R"(#version 330 core
precision lowp float;
out vec4 fragColor;
vec2 fragCoord = gl_FragCoord.xy;
uniform vec2 iMouse;
uniform vec2 iResolution;
uniform float iTime;

float dot2(vec3 v) { return dot(v,v); }

// http://iquilezles.org/www/articles/smin/smin.htm
float smin(float a, float b, float k) {
    float h = clamp(0.5 + 0.5 * (b - a)/k, 0.0, 1.0);
    return mix(b, a, h) - k*h*(1.0 - h);
}

//https://www.iquilezles.org/www/articles/distfunctions/distfunctions.htm
float udTriangle(vec3 p,vec3 a,vec3 b,vec3 c )
{
 vec3 ba = b - a;vec3 pa = p - a;
 vec3 cb = c - b;vec3 pb = p - b;
 vec3 ac = a - c;vec3 pc = p - c;
 vec3 nor = cross( ba, ac );

  return sqrt(
    (sign(dot(cross(ba,nor),pa)) +
     sign(dot(cross(cb,nor),pb)) +
     sign(dot(cross(ac,nor),pc))<2.0)
     ?
     min( min(
     dot2(ba*clamp(dot(ba,pa)/dot2(ba),0.0,1.0)-pa),
     dot2(cb*clamp(dot(cb,pb)/dot2(cb),0.0,1.0)-pb) ),
     dot2(ac*clamp(dot(ac,pc)/dot2(ac),0.0,1.0)-pc) )
     :
     dot(nor,pa)*dot(nor,pa)/dot2(nor) );
}

float sdBox(vec3 rayPosition, vec3 b)
{
    vec3 d = abs(rayPosition)-b;
    return length(max(d,vec3(0))) + min(max(d.x,max(d.y,d.z)),0.0);
}

//https://www.iquilezles.org/www/articles/distfunctions/distfunctions.htm
float udQuad( vec3 rayPosition, vec3 a, vec3 b, vec3 c, vec3 d )
{
  vec3 ba = b - a; vec3 pa = rayPosition - a;
  vec3 cb = c - b; vec3 pb = rayPosition - b;
  vec3 dc = d - c; vec3 pc = rayPosition - c;
  vec3 ad = a - d; vec3 pd = rayPosition - d;
  vec3 nor = cross( ba, ad );

  return sqrt(
    (sign(dot(cross(ba,nor),pa)) +
     sign(dot(cross(cb,nor),pb)) +
     sign(dot(cross(dc,nor),pc)) +
     sign(dot(cross(ad,nor),pd))<3.0)
     ?
     min( min( min(
     dot2(ba*clamp(dot(ba,pa)/dot2(ba),0.0,1.0)-pa),
     dot2(cb*clamp(dot(cb,pb)/dot2(cb),0.0,1.0)-pb) ),
     dot2(dc*clamp(dot(dc,pc)/dot2(dc),0.0,1.0)-pc) ),
     dot2(ad*clamp(dot(ad,pd)/dot2(ad),0.0,1.0)-pd) )
     :
     dot(nor,pa)*dot(nor,pa)/dot2(nor) );
}

float sdGuy(vec3 rayPosition)
{
    //Distance to a sphere = (distanceFrom sample to center minus radius)
    float sphereSize = 0.25;
    
    //Parabola i.e. bouncing
    float t = fract(iTime);
    float y = 4.0*t*(1.0-t);

    vec3 cen = vec3(0.0, y, 0.0);
    return length(rayPosition - cen) - sphereSize;
}

// This is the 3D SDF query. Which is the distance to any object in the scene.
float map(vec3 rayPosition) {
    
    float distanceToSphere = sdGuy(rayPosition);

    vec3 c = vec3(0, 0.05, -0.42);
    vec3 d = vec3(0, 0.0, -0.42);    
    vec3 e = vec3(0, 0.0, 0.42);
    vec3 b = vec3(0, 0.3, 0.42);
    float distanceToQuad = udQuad(rayPosition, c, d, e, b);
 
    //Distance to a plane on the x axis is basically how high the rayPosition is on the y axis
    float planeYPosition = -0.25;
    float distanceToPlane = rayPosition.y - (planeYPosition);
    float distanceToBox = sdBox(rayPosition, vec3(0.01, 0.01, 0.01));

    return min(min(distanceToBox, distanceToPlane), distanceToQuad);

}

// In order to calculate lighting we need to know the surface normal.
// This can be done by comparing changes in distance to the surface in small increments (derivatives)
// Combining the changes in x,y,z produces vector which is the surface gradient
// The surface gradient is equivalent to the surface normal.
vec3 calcNormal(vec3 rayPosition) {
    vec2 e = vec2(0.0001, 0.0);
    
    return normalize(vec3(map(rayPosition+e.xyy)-map(rayPosition-e.xyy),
        map(rayPosition+e.yxy)-map(rayPosition-e.yxy),
        map(rayPosition+e.yyx)-map(rayPosition-e.yyx)));
}

//Raymarching algorithm
float castRay(vec3 rayOrigin, vec3 rayDirection) {    
    float t = 0.0;    
    for(int i=0; i<100; i++) {
        vec3 rayPosition = rayOrigin + t*rayDirection;

        float distanceToObj = map(rayPosition);
        //If we are close enough to an object stop the loop
        if(abs(distanceToObj)<0.001) {
            break;
        }
        t += distanceToObj;
        //If we marched too far stop the loop. Far clipping plane
        if(t > 20.0) break;
    }
    if(t > 20.0) t=-1.0;
    return t;
}


void main()
{
    vec2 pixelCoord = (2.0*fragCoord-iResolution.xy)/iResolution.y;
    //pixelCoord.y = -pixelCoord.y;  //Make coordinate system match OpenGL
    
    // animation based on mouse
    float animation = 10.0*iMouse.x / iResolution.x;

    float fov = 1.5;
    vec3 rayOrigin = vec3(1.0* sin(animation), 0.05, 1.0*cos(animation));
    vec3 cameraTarget = vec3(0.0, 0.0, 0.0);

    vec3 ww = normalize(cameraTarget - rayOrigin);
    vec3 uu = normalize(cross(ww, vec3(0.0, 1.0,0.0)));
    vec3 vv = normalize(cross(uu, ww));
    
    vec3 rayDirection = normalize(pixelCoord.x*uu + pixelCoord.y*vv + fov*ww);

    vec3 backgroundColor = vec3(0.4, 0.75, 1.0); // Sky
    vec3 horizonColor = vec3(0.7, 0.75, 0.8); //De-saturated horizon

    //Adds gradient to backgorund based on the raydirection's y
    // Sky is brighter closer to the horizon
    vec3 finalColor = backgroundColor - 0.7*rayDirection.y;
    finalColor = mix(finalColor, horizonColor, exp(-10.0*rayDirection.y));
    
    // CastRay for Camera
    float t = castRay(rayOrigin, rayDirection);
    // If we hit an object lets do extra work
    if(t>0.0) {
        vec3 rayPosition = rayOrigin + t*rayDirection;
        vec3 surfaceNormal = calcNormal(rayPosition);
        
        //Base material color. (0.2) is the default albeido of grass and generaly used
        //as base material for lighting 
        vec3 material = vec3(0.18);

        //Keylight (Strongest ligth on the scene) intensity 10 in this case the sun/yellow
        //To calculate the surface color with the light you simply need to
        // dot(product) the surface normal vector with the light vector
        vec3 sunDirection = normalize((vec3(0.8, 0.4, 0.2))); //Low close to horizon to enhace shadows 
        vec3 sunColor = vec3(7.0, 4.5, 3.0); //Yellow Keylight intensity 10
        
        //Sky Fill light. Fills the shadows that the key light is not filling
        //Should have an intensity of 1 in this case the sky blue
        vec3 skyDirection = vec3(0.0, 1.0, 0.0); // Sky is up
        vec3 skyColor = vec3(0.5, 0.8, 0.9); //Sky is blue

        //Bouce light. light that is reflected off from other surfaces.
        vec3 bounceDirection = vec3(0.0, -1.0, 0.0); // point up from the bottom
        vec3 bounceColor = vec3(0.7, 0.3, 0.2); //Sun is yellow and surface is white.
        
        // Self intersection avoidance offset
        vec3 offset = surfaceNormal*0.001;
        //cast ray for shadows
        float sunShadow = step(castRay(rayPosition+offset, sunDirection), 0.0);
        float fillLightOffset = 0.5;
        
        float sunDiffuse = clamp(dot(surfaceNormal, sunDirection), 0.0, 1.0);
        float skyDiffuse = clamp(fillLightOffset+fillLightOffset*dot(surfaceNormal, skyDirection), 0.0, 1.0);
        float bounceDiffuse = clamp(fillLightOffset+fillLightOffset*dot(surfaceNormal, bounceDirection), 0.0, 1.0);
        
        finalColor = material * sunColor * sunDiffuse * sunShadow;
        finalColor += material * skyColor * skyDiffuse;
        finalColor += material * bounceColor * bounceDiffuse;

    }

    // Gramma correction
    finalColor = pow(finalColor, vec3(0.4545));
    fragColor = vec4(finalColor, 1.0);
})";

void error_callback(int error, const char* description) {
	fprintf(stderr, "Error: %s\n %i", description, error);
}

int main(void)
{
    glfwSetErrorCallback(error_callback);
    int width = 512;
    int height = 288;

    glm::vec2 screen(width, height);

    float deltaTime = 0.0f; 
    float lastFrame = 0.0f; 

    if(!glfwInit()) {
		fprintf( stderr, "Failed to initialize GLFW\n" );
		getchar();
		return -1;
	}
    glfwWindowHint(GLFW_SAMPLES, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    // glfwWindowHint(GLFW_CLIENT_API, GLFW_OPENGL_ES_API);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

    window = glfwCreateWindow(width, height, "GLFWLive", nullptr, nullptr);
    if (!window)
    {
		fprintf( stderr, "Failed to open GLFW window. If you have an Intel GPU, they are not 3.3 compatible. Try the 2.1 version of the tutorials.\n" );
		getchar();
		glfwTerminate();
		return -1;
    }
    glfwMakeContextCurrent(window);

	// Initialize GLEW
	if (glewInit() != GLEW_OK) {
		fprintf(stderr, "Failed to initialize GLEW\n");
		getchar();
		glfwTerminate();
		return -1;
	}

    glfwSetInputMode(window, GLFW_STICKY_KEYS, GL_TRUE);
    // glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);

    float quadVerts[] = {
        -1.0, -1.0,     0.0, 0.0,
        -1.0, 1.0,      0.0, 1.0,
        1.0, -1.0,      1.0, 0.0,

        1.0, -1.0,      1.0, 0.0,
        -1.0, 1.0,      0.0, 1.0,
        1.0, 1.0,       1.0, 1.0
    };

    GLuint VAO;
    glGenVertexArrays(1, &VAO);
    glBindVertexArray(VAO);

    GLuint VBO;
    glGenBuffers(1, &VBO);
    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(quadVerts), quadVerts, GL_STATIC_DRAW);

    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(0);

    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), reinterpret_cast<void*>(2 * sizeof(float)));
    glEnableVertexAttribArray(1);

    glBindVertexArray(0);


    GLuint framebuffer;
    glGenFramebuffers(1, &framebuffer);
    glBindFramebuffer(GL_FRAMEBUFFER, framebuffer); 

    GLuint texColor;
    glGenTextures(1, &texColor);
    glBindTexture(GL_TEXTURE_2D, texColor);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, 0);
    glBindTexture(GL_TEXTURE_2D, 0);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, texColor, 0);

    glBindFramebuffer(GL_FRAMEBUFFER, 0);


    //vertex shader
    unsigned int vertexShader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertexShader, 1, &vertexShaderSource, NULL);
    glCompileShader(vertexShader);

    // fragment shader
    unsigned int fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragmentShader, 1, &fragmentShaderSource2, NULL);
    glCompileShader(fragmentShader);
    // check for shader compile errors

    // link shaders
    unsigned int shaderProgram = glCreateProgram();
    glAttachShader(shaderProgram, vertexShader);
    glAttachShader(shaderProgram, fragmentShader);
    glLinkProgram(shaderProgram);
    // check for linking errors

    glDeleteShader(vertexShader);
    glDeleteShader(fragmentShader);

    glUseProgram(shaderProgram);
    glUniform2fv(glGetUniformLocation(shaderProgram, "iResolution"), 1, &screen[0]);

    const GLubyte* renderer = glGetString (GL_RENDERER); // get renderer string
    const GLubyte* version = glGetString (GL_VERSION); // version as a string
    printf("Renderer: %s\n", renderer);
    printf("OpenGL version supported %s\n", version);
    float iTime = 0;
    while (!glfwWindowShouldClose(window))
    {

        float currentFrame = glfwGetTime();
        deltaTime = currentFrame - lastFrame;
        printf("%f\n", deltaTime);
        iTime += .01;
        lastFrame = currentFrame; 

        if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS)
        {
            glfwSetWindowShouldClose(window, true);
        }

        glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);


        glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT);

        glBindFramebuffer(GL_FRAMEBUFFER, 0);
        glUseProgram(shaderProgram);
        glUniform1f(glGetUniformLocation(shaderProgram, "iTime"), iTime);
        glBindVertexArray(VAO);
        glDrawArrays(GL_TRIANGLES, 0, 6);


        glfwSwapBuffers(window);
        glfwPollEvents();

    }
    glfwTerminate();
    // cleanup
}

