//--------------------------------------------------------------------------------------
#include "stdafx.h"

#include <windows.h>
#include <d3d11_1.h>
#include <d3dcompiler.h>
#include <directxmath.h>
#include <directxcolors.h>
#include "resource.h"

//d3d11.lib;d3dcompiler.lib;dxguid.lib;winmm.lib;comctl32.lib;%(AdditionalDependencies)

#pragma comment (lib, "d3d11")
#pragma comment (lib, "d3dcompiler")

using namespace DirectX;

#define TEST_HR_IMPL2(n) hr_tester##n
#define TEST_HR_IMPL1(n) TEST_HR_IMPL2(n)
#define TEST_HR hr_tester TEST_HR_IMPL1(__COUNTER__) = 

namespace
{
    struct hr_exception : std::exception 
    {
        HRESULT hresult;

        explicit hr_exception (HRESULT hr)
            :   std::exception ("hr_exception")
            ,   hresult (hr)
        {
        }
    };

    struct hr_tester 
    {
        inline hr_tester (HRESULT hr)
        {
            if (FAILED (hr))
            {
                throw hr_exception (hr);
            }
        }
    };



    template<typename TInterface>
    struct com_out_ptr;

    template<typename TInterface>
    struct com_ptr
    {
        explicit com_ptr (TInterface* ptr = nullptr) throw ()
            :   m_ptr (ptr)
        {
            increase ();
        }

        com_ptr (com_ptr const & cp) throw ()
            :   m_ptr (cp.m_ptr)
        {
            increase ();
        }

        com_ptr (com_ptr && cp) throw ()
            :   m_ptr (cp.m_ptr)
        {
            cp.m_ptr = nullptr;
        }

        ~com_ptr () throw ()
        {
            release ();
            m_ptr = nullptr;
        }

        inline void swap (com_ptr & cp) throw ()
        {
            auto tmp    = m_ptr     ;
            m_ptr       = cp.m_ptr  ;
            cp.m_ptr    = tmp       ;
        }

        void reset (TInterface* ptr = nullptr) throw ()
        {
            release ();
            m_ptr = nullptr;

            m_ptr = ptr;;
            increase ();
        }

        com_ptr& operator= (com_ptr const & cp) throw ()
        {
            if (this == &cp)
            {
                return *this;
            }

            reset (cp.m_ptr);

            return *this;
        }

        com_ptr& operator= (com_ptr && cp) throw ()
        {
            if (this == &cp)
            {
                return *this;
            }

            m_ptr   = cp.m_ptr  ;
            cp.m_ptr= nullptr   ;

            return *this;
        }

        com_out_ptr<TInterface> get_out_ptr () throw ()
        {
            return com_out_ptr<TInterface> (*this);
        }

        inline TInterface* get () const throw () 
        {
            return m_ptr;
        }

        inline TInterface* operator-> () const throw () 
        {
            assert (m_ptr);
            return m_ptr;
        }

        explicit operator bool () throw ()
        {
            return m_ptr != nullptr;
        }

    private:
        inline void increase () throw ()
        {
            if (m_ptr)
            {
                m_ptr->AddRef ();
            }
        }

        inline void release () throw ()
        {
            if (m_ptr)
            {
                m_ptr->Release ();
            }
        }

        TInterface * m_ptr;
    };

    template<typename TInterface>
    struct com_out_ptr
    {
        explicit com_out_ptr (com_ptr<TInterface> & cp)
            :   m_target    (&cp)
            ,   m_ptr       (nullptr)
        {
        }

        com_out_ptr (com_out_ptr && cop)
            :   m_target    (cop.m_target)
            ,   m_ptr       (cop.m_ptr)
        {
            cop.m_target= nullptr;
            cop.m_ptr   = nullptr;
        }

        com_out_ptr& operator= (com_out_ptr && cop)
        {
            if (this == &cop)
            {
                return *this;
            }

            m_target    = cop.m_target  ;
            m_ptr       = cop.m_ptr     ;

            cop.m_target= nullptr       ;
            cop.m_ptr   = nullptr       ;

            return *this;
        }

        ~com_out_ptr () throw ()
        {
            if (m_target)
            {
                m_target->reset (m_ptr);
            }
        }


        operator TInterface** () throw ()
        {
            return &m_ptr;
        }
        
        operator void** () throw ()
        {
            return reinterpret_cast<void**> (&m_ptr);
        }
        
    private:
        com_out_ptr (com_out_ptr const & cop);
        com_out_ptr & operator= (com_out_ptr const & cop);

        com_ptr<TInterface> * m_target  ;
        TInterface          * m_ptr     ;
    };

    template<typename TPredicate>
    struct scope_guard
    {
        explicit inline scope_guard (TPredicate const & predicate)
            :   predicate   (predicate)
        {
        }

        explicit inline scope_guard (TPredicate&& predicate) throw ()
            :   predicate   (std::move (predicate))
        {
        }

        inline ~scope_guard () throw ()
        {
            predicate ();
        }

        // No implementation needed due to RVO
        inline scope_guard (scope_guard &&)  throw ()   ;

    private:
        scope_guard ()                                  ;
        scope_guard (scope_guard const &)               ;
                                                        ;
        scope_guard& operator= (scope_guard const &)    ;
        scope_guard& operator= (scope_guard &&)         ;

        //scope_guard ()                                  = delete;
        //scope_guard (scope_guard const &)               = delete;
        //scope_guard (scope_guard &&)                    = delete;

        //scope_guard& operator= (scope_guard const &)    = delete;
        //scope_guard& operator= (scope_guard &&)         = delete;

        TPredicate  predicate   ;
    };


    template<typename TPredicate>
    inline scope_guard<TPredicate> on_exit (TPredicate&& predicate)
    {
        return scope_guard<TPredicate> (std::forward<TPredicate> (predicate));
    }

    typedef std::vector<BYTE> bytes;

    std::wstring get_root_path ()
    {
        wchar_t path[MAX_PATH];

        auto length = GetModuleFileName (
                nullptr
            ,   path
            ,   MAX_PATH
            );

        std::wstring full_path (path, path + length);

        auto last_slash = full_path.rfind (L'\\');

        if (last_slash == std::wstring::npos)
        {
            return full_path;
        }

        return full_path.substr (0, last_slash + 1);

    }

    bytes load_bytes (std::wstring const & file_path)
    {
        auto path = get_root_path ();

        auto full_path = path + file_path;

        auto hnd = CreateFile (
                full_path.c_str ()
            ,   GENERIC_READ
            ,   FILE_SHARE_READ
            ,   nullptr
            ,   OPEN_EXISTING
            ,   FILE_ATTRIBUTE_NORMAL
            ,   nullptr
            );
        if  (hnd == INVALID_HANDLE_VALUE)
        {
            return bytes ();
        }

        auto close_handle = on_exit ([=]() {CloseHandle (hnd);});

        DWORD const buffer_size = 4096  ;

        DWORD       bytes_read  = 0     ;
        BYTE        buffer[buffer_size] ;

        bytes result;

        while (
                ReadFile (
                        hnd
                    ,   buffer
                    ,   buffer_size
                    ,   &bytes_read
                    ,   nullptr
                    )
            &&  bytes_read > 0
            )
        {
            result.insert (
                    result.end ()
                ,   buffer
                ,   buffer + bytes_read
                );
        }

        return result;
    }
}

namespace
{
    struct ModelViewProjectionConstantBuffer
    {
        XMFLOAT4X4 model;
        XMFLOAT4X4 view;
        XMFLOAT4X4 projection;
    };

    struct MandelbrotPos
    {
        XMFLOAT3 pos   ;
        XMFLOAT3 normal;
        XMFLOAT2 texpos;
    };

    struct device_independent_resources
    {
        typedef std::unique_ptr<device_independent_resources>   ptr ;
        HINSTANCE               hinst   ;
        HWND                    hwnd    ;
    };

    struct device_dependent_resources
    {
        typedef std::unique_ptr<device_dependent_resources>     ptr ;

        ~device_dependent_resources () throw ()
        {
            if (device_context)
            {
                device_context->ClearState ();
            }
        }

        D3D_DRIVER_TYPE                     driver_type             = D3D_DRIVER_TYPE_NULL  ;
        D3D_FEATURE_LEVEL                   feature_level           = D3D_FEATURE_LEVEL_11_0;

        com_ptr<ID3D11Device            >   device                  ;
        com_ptr<ID3D11DeviceContext     >   device_context          ;
        com_ptr<IDXGISwapChain          >   swap_chain              ;
        com_ptr<ID3D11Texture2D         >   back_buffer             ;
        com_ptr<ID3D11RenderTargetView  >   render_target_view      ;
        com_ptr<ID3D11VertexShader      >   vertex_shader           ;
        com_ptr<ID3D11PixelShader       >   pixel_shader            ;
        com_ptr<ID3D11InputLayout       >   input_layout            ;

        com_ptr<ID3D11Buffer            >   view_buffer             ;
        com_ptr<ID3D11Buffer            >   vertex_buffer           ;
    };

    struct size_dependent_resources
    {
        typedef std::unique_ptr<size_dependent_resources>       ptr ;

        ModelViewProjectionConstantBuffer   view                    ;
    };
    
    device_independent_resources::ptr   dir ;
    device_dependent_resources::ptr     ddr ;
    size_dependent_resources::ptr       sdr ;


}

//--------------------------------------------------------------------------------------
// Forward declarations
//--------------------------------------------------------------------------------------
HRESULT             init_window (HINSTANCE hInstance, int nCmdShow);
HRESULT             init_device ();
LRESULT CALLBACK    wnd_proc (HWND, UINT, WPARAM, LPARAM);
void                render ();


//--------------------------------------------------------------------------------------
// Entry point to the program. Initializes everything and goes into a message processing 
// loop. Idle time is used to render the scene.
//--------------------------------------------------------------------------------------
int WINAPI wWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPWSTR lpCmdLine, int nCmdShow)
{
    UNREFERENCED_PARAMETER (hPrevInstance);
    UNREFERENCED_PARAMETER (lpCmdLine);

    try
    {
        dir = std::make_unique<device_independent_resources> ();

        TEST_HR init_window (hInstance, nCmdShow);

        TEST_HR init_device ();

        // Main message loop
        MSG msg = {0};
        while (WM_QUIT != msg.message)
        {
            if (PeekMessage (&msg, nullptr, 0, 0, PM_REMOVE))
            {
                TranslateMessage (&msg);
                DispatchMessage (&msg);
            }
            else
            {
                render ();
            }
        }

        sdr.release ();
        ddr.release ();
        dir.release ();

        return (int) msg.wParam;
    }
    catch (std::exception const & e)
    {
       OutputDebugString (L"Caught exception: ");
       OutputDebugStringA (e.what ());
       OutputDebugString (L"\r\n");
       return 999;
    }
    catch (...)
    {
       OutputDebugString (L"Caught exception: unknown\r\n");
       return 999;
    }
}


//--------------------------------------------------------------------------------------
// Register class and create window
//--------------------------------------------------------------------------------------
HRESULT init_window (HINSTANCE hInstance, int nCmdShow)
{
    // Register class
    WNDCLASSEX wcex;
    wcex.cbSize         = sizeof (WNDCLASSEX)               ;
    wcex.style          = CS_HREDRAW | CS_VREDRAW           ;
    wcex.lpfnWndProc    = wnd_proc                          ;
    wcex.cbClsExtra     = 0                                 ;
    wcex.cbWndExtra     = 0                                 ;
    wcex.hInstance      = hInstance                         ;
    wcex.hIcon          = nullptr                           ;
    wcex.hCursor        = LoadCursor (nullptr, IDC_ARROW)   ;
    wcex.hbrBackground  = (HBRUSH)(COLOR_WINDOW + 1)        ;
    wcex.lpszMenuName   = nullptr                           ;
    wcex.lpszClassName  = L"MandelbrotWindowClass"          ;
    wcex.hIconSm        = nullptr                           ;
    if (!RegisterClassEx (&wcex))
    {
        return E_FAIL;
    }

    dir->hinst = hInstance;

    RECT rc = { 0, 0, 1024, 768 };
    AdjustWindowRect (&rc, WS_OVERLAPPEDWINDOW, FALSE);
    dir->hwnd = CreateWindow ( 
            L"MandelbrotWindowClass" 
        ,   L"Mandelbrot"
        ,   WS_OVERLAPPEDWINDOW
        ,   CW_USEDEFAULT
        ,   CW_USEDEFAULT
        ,   rc.right - rc.left
        ,   rc.bottom - rc.top
        ,   nullptr
        ,   nullptr
        ,   hInstance
        ,   nullptr 
        );

    if (!dir->hwnd)
    {
        return E_FAIL;
    }

    ShowWindow (dir->hwnd, nCmdShow);

    return S_OK;
}

//--------------------------------------------------------------------------------------
// Create Direct3D device and swap chain
//--------------------------------------------------------------------------------------
HRESULT init_device ()
{
    ddr = std::make_unique<device_dependent_resources> ();
    sdr = std::make_unique<size_dependent_resources> ();

    RECT rc = {};
    GetClientRect (dir->hwnd, &rc);

    UINT width = rc.right - rc.left;
    UINT height = rc.bottom - rc.top;

    UINT createDeviceFlags = 0;
#ifdef _DEBUG
    createDeviceFlags |= D3D11_CREATE_DEVICE_DEBUG;
#endif

    D3D_DRIVER_TYPE driverTypes[] =
    {
        D3D_DRIVER_TYPE_HARDWARE,
        D3D_DRIVER_TYPE_WARP,
        D3D_DRIVER_TYPE_REFERENCE,
    };
    UINT numDriverTypes = ARRAYSIZE (driverTypes);

    D3D_FEATURE_LEVEL featureLevels[] =
    {
        D3D_FEATURE_LEVEL_11_1,
        D3D_FEATURE_LEVEL_11_0,
        D3D_FEATURE_LEVEL_10_1,
        D3D_FEATURE_LEVEL_10_0,
    };
	UINT numFeatureLevels = ARRAYSIZE (featureLevels);

    DXGI_SWAP_CHAIN_DESC sd;
    ZeroMemory (&sd, sizeof (sd));
    sd.BufferCount                          = 1                                 ;
    sd.BufferDesc.Width                     = width                             ;
    sd.BufferDesc.Height                    = height                            ;
    sd.BufferDesc.Format                    = DXGI_FORMAT_R8G8B8A8_UNORM        ;
    sd.BufferDesc.RefreshRate.Numerator     = 60                                ;
    sd.BufferDesc.RefreshRate.Denominator   = 1                                 ;
    sd.BufferUsage                          = DXGI_USAGE_RENDER_TARGET_OUTPUT   ;
    sd.OutputWindow                         = dir->hwnd                         ;
    sd.SampleDesc.Count                     = 1                                 ;
    sd.SampleDesc.Quality                   = 0                                 ;
    sd.Windowed                             = TRUE                              ;

    for (UINT driverTypeIndex = 0; driverTypeIndex < numDriverTypes; ++driverTypeIndex)
    {
        ddr->driver_type = driverTypes[driverTypeIndex];
        HRESULT create_hr = D3D11CreateDeviceAndSwapChain (
                nullptr
            ,   ddr->driver_type 
            ,   nullptr
            ,   createDeviceFlags
            ,   featureLevels
            ,   numFeatureLevels
            ,   D3D11_SDK_VERSION
            ,   &sd
            ,   ddr->swap_chain.get_out_ptr ()
            ,   ddr->device.get_out_ptr ()
            ,   &ddr->feature_level
            ,   ddr->device_context.get_out_ptr ()
            );

        if (create_hr == E_INVALIDARG)
        {
            // DirectX 11.0 platforms will not recognize D3D_FEATURE_LEVEL_11_1 so we need to retry without it
            create_hr = D3D11CreateDeviceAndSwapChain ( 
                    nullptr
                ,   ddr->driver_type 
                ,   nullptr
                ,   createDeviceFlags
                ,   &featureLevels[1]
                ,   numFeatureLevels - 1
                ,   D3D11_SDK_VERSION
                ,   &sd
                ,   ddr->swap_chain.get_out_ptr ()
                ,   ddr->device.get_out_ptr ()
                ,   &ddr->feature_level
                ,   ddr->device_context.get_out_ptr ()
                );
        }

        if (SUCCEEDED (create_hr))
        {
            break;
        }
    }

    // Create a render target view
    {
        TEST_HR ddr->swap_chain->GetBuffer ( 
                0
            ,   __uuidof (ID3D11Texture2D)
            ,   ddr->back_buffer.get_out_ptr ()
            );

        TEST_HR ddr->device->CreateRenderTargetView (
                ddr->back_buffer.get ()
            ,   nullptr
            ,   ddr->render_target_view.get_out_ptr ()
            );
    }

    ID3D11RenderTargetView * render_targets[] =
    {
        ddr->render_target_view.get (),
    };

    ddr->device_context->OMSetRenderTargets (
            ARRAYSIZE (render_targets)
        ,   render_targets
        ,   nullptr
        );

    // Setup the viewport
    D3D11_VIEWPORT vp               ;
    vp.Width        = (FLOAT)width  ;
    vp.Height       = (FLOAT)height ;
    vp.MinDepth     = 0.0f          ;
    vp.MaxDepth     = 1.0f          ;
    vp.TopLeftX     = 0             ;
    vp.TopLeftY     = 0             ;

    ddr->device_context->RSSetViewports (1, &vp);

    auto vertex_shader_bytes= load_bytes (L"SceneVertexShader.cso");
    auto pixel_bytes        = load_bytes (L"ScenePixelShader.cso");

    if (vertex_shader_bytes.empty ())
    {
        return E_FAIL;
    }

    if (pixel_bytes.empty ())
    {
        return E_FAIL;
    }

    TEST_HR ddr->device->CreateVertexShader (
            &vertex_shader_bytes.front ()
        ,   vertex_shader_bytes.size ()
        ,   nullptr
        ,   ddr->vertex_shader.get_out_ptr ()
        );

	TEST_HR ddr->device->CreatePixelShader( 
            &pixel_bytes.front ()
        ,   pixel_bytes.size ()
        ,   nullptr
        ,   ddr->pixel_shader.get_out_ptr ()
        );

    // Define the input layout
    static const D3D11_INPUT_ELEMENT_DESC vertexDesc [] =
    {
        { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0,  0, D3D11_INPUT_PER_VERTEX_DATA, 0 },
        { "NORMAL"  , 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12, D3D11_INPUT_PER_VERTEX_DATA, 0 },
        { "TEXCOORD", 0, DXGI_FORMAT_R32G32_FLOAT,    0, 24, D3D11_INPUT_PER_VERTEX_DATA, 0 },
    };

    // Create the input layout
	TEST_HR ddr->device->CreateInputLayout ( 
            vertexDesc
        ,   ARRAYSIZE (vertexDesc)
        ,   &vertex_shader_bytes.front ()
        ,   vertex_shader_bytes.size ()
        ,   ddr->input_layout.get_out_ptr ()
        );

    // Set the input layout
    ddr->device_context->IASetInputLayout (ddr->input_layout.get ());

    // Create vertex buffer
    MandelbrotPos vertices[] =
    {
        {XMFLOAT3 (-1, -0.5f, 0.5f), XMFLOAT3 ( 0.0f, 0.0f,-1.0f), XMFLOAT2 ( 0, 1)},
        {XMFLOAT3 ( 0, -0.5f, 0.5f), XMFLOAT3 ( 0.0f, 0.0f,-1.0f), XMFLOAT2 ( 1, 1)},
        {XMFLOAT3 ( 0,  0.5f, 0.5f), XMFLOAT3 ( 0.0f, 0.0f,-1.0f), XMFLOAT2 ( 1, 0)},
        {XMFLOAT3 (-1,  0.5f, 0.5f), XMFLOAT3 ( 0.0f, 0.0f,-1.0f), XMFLOAT2 ( 0, 0)},
                                                                            
        {XMFLOAT3 ( 0, -0.5f, 0.5f), XMFLOAT3 ( 0.0f, 0.0f,-1.0f), XMFLOAT2 ( 0, 1)},
        {XMFLOAT3 ( 1, -0.5f, 0.5f), XMFLOAT3 ( 0.0f, 0.0f,-1.0f), XMFLOAT2 ( 1, 1)},
        {XMFLOAT3 ( 1,  0.5f, 0.5f), XMFLOAT3 ( 0.0f, 0.0f,-1.0f), XMFLOAT2 ( 1, 0)},
        {XMFLOAT3 ( 0,  0.5f, 0.5f), XMFLOAT3 ( 0.0f, 0.0f,-1.0f), XMFLOAT2 ( 0, 0)},
    };

    {
        CD3D11_BUFFER_DESC viewBufferDesc (sizeof (ModelViewProjectionConstantBuffer), D3D11_BIND_CONSTANT_BUFFER);
        TEST_HR ddr->device->CreateBuffer (
                &viewBufferDesc
            ,   nullptr
            ,   ddr->view_buffer.get_out_ptr ()
            );
    }

    {
        D3D11_SUBRESOURCE_DATA vertexBufferData     = {0}       ;
        vertexBufferData.pSysMem                    = vertices  ;
        vertexBufferData.SysMemPitch                = 0         ;
        vertexBufferData.SysMemSlicePitch           = 0         ;
        CD3D11_BUFFER_DESC vertexBufferDesc (
                sizeof (vertices)
            ,   D3D11_BIND_VERTEX_BUFFER
            );

        TEST_HR ddr->device->CreateBuffer (
                &vertexBufferDesc
            ,   &vertexBufferData
            ,   ddr->vertex_buffer.get_out_ptr ()
            );
    }

    ID3D11Buffer* buffers[] =
    {
        ddr->vertex_buffer.get (),
    };

    // Set vertex buffer
    UINT stride = sizeof (MandelbrotPos);
    UINT offset = 0;
    ddr->device_context->IASetVertexBuffers ( 
            0
        ,   ARRAYSIZE (buffers)
        ,   buffers
        ,   &stride
        ,   &offset 
        );

    // Set primitive topology
    ddr->device_context->IASetPrimitiveTopology (D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);


    // Size dependent resources

    const XMVECTORF32 eye   = { 0.0f, 0.0f, 1.5f, 0.0f };
    const XMVECTORF32 at    = { 0.0f, 0.0f, 0.0f, 0.0f };
    const XMVECTORF32 up    = { 0.0f, 1.0f, 0.0f, 0.0f };

    XMMATRIX perspectiveMatrix = XMMatrixOrthographicRH (
        2,
        1,
        -10,
        +10
        );

    XMStoreFloat4x4 (
            &sdr->view.projection
        ,   perspectiveMatrix
        );

    XMStoreFloat4x4 (
            &sdr->view.view
        ,   XMMatrixTranspose (XMMatrixLookAtRH (eye, at, up))
        );

    XMStoreFloat4x4 (
            &sdr->view.model
        ,   XMMatrixRotationY (0)
        );

    return S_OK;
}


//--------------------------------------------------------------------------------------
// Called every time the application receives a message
//--------------------------------------------------------------------------------------
LRESULT CALLBACK wnd_proc (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    PAINTSTRUCT ps;
    HDC hdc;

    switch (message)
    {
        case WM_PAINT:
            hdc = BeginPaint (hWnd, &ps);
            EndPaint (hWnd, &ps);
            break;

        case WM_DESTROY:
            PostQuitMessage (0);
            break;

        default:
            return DefWindowProc (hWnd, message, wParam, lParam);
    }

    return 0;
}


//--------------------------------------------------------------------------------------
// render a frame
//--------------------------------------------------------------------------------------
void render ()
{
    if (!ddr)
    {
        return;
    }

    // Clear the back buffer 
    ddr->device_context->ClearRenderTargetView (ddr->render_target_view.get (), Colors::MidnightBlue);

    // render a triangle
	ddr->device_context->VSSetShader (ddr->vertex_shader.get (), nullptr, 0);
	ddr->device_context->PSSetShader (ddr->pixel_shader.get (), nullptr, 0);
    ddr->device_context->Draw (3, 0);

    // Present the information rendered to the back buffer to the front buffer (the screen)
    ddr->swap_chain->Present (0, 0);
}
