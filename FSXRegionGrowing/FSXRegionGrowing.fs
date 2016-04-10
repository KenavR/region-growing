namespace FSXRegionGrowing

open System
open System.Drawing
open System.Drawing.Imaging

type Pixel(x:int, y:int, position:int) =
    member this.X = x
    member this.Y = y
    member this.Position = position

type Homogenity(r:int, g:int, b:int) =
    member this.R = r
    member this.G = g
    member this.B = b

type RGB(r:int, g:int, b:int) =
    member this.R = r
    member this.G = g
    member this.B = b

module RG =
    let pixelFromImage (image:Bitmap) =
        let Width = image.Width
        let Height = image.Height
        let rect = new Rectangle(0,0,Width,Height)

        // Lock the image for access
        let data = image.LockBits(rect, ImageLockMode.ReadOnly, image.PixelFormat)

        // Copy the data
        let ptr = data.Scan0
        let stride = data.Stride
        let bytes = stride * data.Height
        let values : byte[] = Array.zeroCreate bytes
        System.Runtime.InteropServices.Marshal.Copy(ptr,values,0,bytes)

        // Unlock the image
        image.UnlockBits(data)

        let pixelSize = 4 // <-- calculate this from the PixelFormat

        // Create and return a 3D-array with the copied data
        Array3D.init 3 Width Height (fun i x y ->
            values.[stride * y + x * pixelSize + i])

    let private getNeighbours (pixelMap:Pixel[,]) (pixel:Pixel) =
        let x = pixel.X
        let y = pixel.Y
        let neighbours = 
            [| pixelMap.[x-1, y]; pixelMap.[x, y-1]; pixelMap.[x+1, y+1]; pixelMap.[x, y+1] |] //get all neighbours
            |> Array.filter (fun p -> p.Position = 0) //filter to unchecked ones
        neighbours

    let private isHomogen (pixel:RGB) (homogenity:Homogenity) = 
        match pixel with
        | pixel when 
            pixel.R > homogenity.R 
            && pixel.G > homogenity.G 
            && pixel.B > homogenity.B -> true
        | _ -> false

    let private getAnalyzedPixel (imgPixels:byte[,,]) (pixel:Pixel):Pixel = 
        let color = new RGB(int imgPixels.[0, pixel.X, pixel.Y],int imgPixels.[1, pixel.X, pixel.Y],int imgPixels.[2, pixel.X, pixel.Y]) //image.GetPixel(pixel.X, pixel.Y)
        let homogenity = new Homogenity(230, 230, 230)
        let isHomo = isHomogen color homogenity
        match isHomo with
        | true -> new Pixel(pixel.X, pixel.Y, 1)
        | _ -> new Pixel(pixel.X, pixel.Y, -1)

    let private updatePixelMap (pixelMap:Pixel[,]) (pixel:Pixel):Pixel[,] =
        pixelMap.[pixel.X, pixel.Y] <- pixel
        pixelMap

    let private transformUnchecked (pixel:Pixel) =
        match pixel.Position with
        | 0 -> new Pixel(pixel.X, pixel.Y, -1)
        | _ -> pixel

    let rec private regionGrowing (pixelMap:Pixel[,]) (imgPixels:byte[,,]) (pixel:Pixel) = 
        async {
            let neighbours = getNeighbours pixelMap pixel

            let analyzedNeighbours = 
                neighbours 
                |> Array.map (fun p -> getAnalyzedPixel imgPixels p )
                       
            let updatedMap = analyzedNeighbours |> Array.map (fun p -> updatePixelMap pixelMap p)
        
            do! [for p in (analyzedNeighbours |> Array.filter (fun p -> p.Position = 1)) -> regionGrowing pixelMap imgPixels p ] 
                         |> Async.Parallel |> Async.Ignore 

    }


    let analyzeImage (path:string) (x:int) (y:int) = 
        let img = new Bitmap(Image.FromFile(path))
        let imgPixels = pixelFromImage img
        let mutable pixelMap = Array2D.init img.Width img.Height (fun x y -> new Pixel(x, y, 0))
        let seedPixel = new Pixel(x, y, 1)
        Async.RunSynchronously (regionGrowing pixelMap imgPixels seedPixel) 
        pixelMap <- Array2D.map transformUnchecked pixelMap
        pixelMap |> Array2D.map (fun p -> p.Position)


       // library functions 





//Testing
//let result = (analyzeImage "C:\\Users\\Rene\\Desktop\\projects\\FH\\PPRG\\test_white.jpg" 250 250) 
//printfn "%A" result;



