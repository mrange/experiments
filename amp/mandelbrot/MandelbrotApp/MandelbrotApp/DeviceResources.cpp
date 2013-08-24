#include "pch.h"
#include "DeviceResources.h"

#include "Common\DirectXHelper.h"				// For ThrowIfFailed
#include <windows.ui.xaml.media.dxinterop.h>	// For SwapChainBackgroundPanel native methods

using namespace MandelbrotApp;

using namespace D2D1;
using namespace DirectX;
using namespace Microsoft::WRL;
using namespace Windows::Graphics::Display;
using namespace Windows::UI::Core;
using namespace Windows::UI::Xaml::Controls;

// Constants used to calculate screen rotations
namespace ScreenRotation
{
	// 0-degree Z-rotation
	static const XMFLOAT4X4 Rotation0( 
		1.0f, 0.0f, 0.0f, 0.0f,
		0.0f, 1.0f, 0.0f, 0.0f,
		0.0f, 0.0f, 1.0f, 0.0f,
		0.0f, 0.0f, 0.0f, 1.0f
		);

	// 90-degree Z-rotation
	static const XMFLOAT4X4 Rotation90(
		0.0f, 1.0f, 0.0f, 0.0f,
		-1.0f, 0.0f, 0.0f, 0.0f,
		0.0f, 0.0f, 1.0f, 0.0f,
		0.0f, 0.0f, 0.0f, 1.0f
		);

	// 180-degree Z-rotation
	static const XMFLOAT4X4 Rotation180(
		-1.0f, 0.0f, 0.0f, 0.0f,
		0.0f, -1.0f, 0.0f, 0.0f,
		0.0f, 0.0f, 1.0f, 0.0f,
		0.0f, 0.0f, 0.0f, 1.0f
		);

	// 270-degree Z-rotation
	static const XMFLOAT4X4 Rotation270( 
		0.0f, -1.0f, 0.0f, 0.0f,
		1.0f, 0.0f, 0.0f, 0.0f,
		0.0f, 0.0f, 1.0f, 0.0f,
		0.0f, 0.0f, 0.0f, 1.0f
		);
};

// Constructor for DeviceResources.
DeviceResources::DeviceResources() : 
	m_screenViewport(),
	m_d3dFeatureLevel(D3D_FEATURE_LEVEL_9_1),
	m_d3dRenderTargetSize(),
	m_outputSize(),
	m_orientation(DisplayOrientations::None),
	m_dpi(-1.0f),
	m_deviceNotify(nullptr)
{
	CreateDeviceIndependentResources();
	CreateDeviceResources();
}

// Configures resources that don't depend on the Direct3D device.
void DeviceResources::CreateDeviceIndependentResources()
{
	// Initialize Direct2D resources
	D2D1_FACTORY_OPTIONS options;
	ZeroMemory(&options, sizeof(D2D1_FACTORY_OPTIONS));

#if defined(_DEBUG)
	// If the project is in a debug build, enable Direct2D debugging via SDK Layers.
	options.debugLevel = D2D1_DEBUG_LEVEL_INFORMATION;
#endif

	// Initialize the Direct2D Factory
	DX::ThrowIfFailed(
		D2D1CreateFactory(
			D2D1_FACTORY_TYPE_SINGLE_THREADED,
			__uuidof(ID2D1Factory2),
			&options,
			&m_d2dFactory
			)
		);

	// Initialize the DirectWrite Factory
	DX::ThrowIfFailed(
		DWriteCreateFactory(
			DWRITE_FACTORY_TYPE_SHARED,
			__uuidof(IDWriteFactory2),
			&m_dwriteFactory
			)
		);

	// Initialize the Windows Imaging Component (WIC) Factory
	DX::ThrowIfFailed(
		CoCreateInstance(
			CLSID_WICImagingFactory2,
			nullptr,
			CLSCTX_INPROC_SERVER,
			IID_PPV_ARGS(&m_wicFactory)
			)
		);
}

// Configures the Direct3D device, and stores handles to it and the device context.
void DeviceResources::CreateDeviceResources() 
{
	// This flag adds support for surfaces with a different color channel ordering
	// than the API default. It is required for compatibility with Direct2D.
	UINT creationFlags = D3D11_CREATE_DEVICE_BGRA_SUPPORT;

#if defined(_DEBUG)
	if (DX::SdkLayersAvailable())
	{
		// If the project is in a debug build, enable debugging via SDK Layers with this flag.
		creationFlags |= D3D11_CREATE_DEVICE_DEBUG;
	}
#endif

	// This array defines the set of DirectX hardware feature levels this app will support.
	// Note the ordering should be preserved.
	// Don't forget to declare your application's minimum required feature level in its
	// description.  All applications are assumed to support 9.1 unless otherwise stated.
	D3D_FEATURE_LEVEL featureLevels[] = 
	{
		D3D_FEATURE_LEVEL_11_1,
		D3D_FEATURE_LEVEL_11_0,
		D3D_FEATURE_LEVEL_10_1,
		D3D_FEATURE_LEVEL_10_0,
		D3D_FEATURE_LEVEL_9_3,
		D3D_FEATURE_LEVEL_9_2,
		D3D_FEATURE_LEVEL_9_1
	};

	// Create the Direct3D 11 API device object and a corresponding context.
	ComPtr<ID3D11Device> device;
	ComPtr<ID3D11DeviceContext> context;

	HRESULT hr = D3D11CreateDevice(
		nullptr,					// Specify nullptr to use the default adapter.
		D3D_DRIVER_TYPE_HARDWARE,	// Create a device using the hardware graphics driver.
		0,							// Should be 0 unless the driver is D3D_DRIVER_TYPE_SOFTWARE.
		creationFlags,				// Set debug and Direct2D compatibility flags.
		featureLevels,				// List of feature levels this app can support.
		ARRAYSIZE(featureLevels),	// Size of the list above.
		D3D11_SDK_VERSION,			// Always set this to D3D11_SDK_VERSION for Windows Store apps.
		&device,					// Returns the Direct3D device created.
		&m_d3dFeatureLevel,			// Returns feature level of device created.
		&context					// Returns the device immediate context.
		);

	if (FAILED(hr))
	{
		// If the initialization fails, fall back to the WARP device.
		// For more information on WARP, see: 
		// http://go.microsoft.com/fwlink/?LinkId=286690
		DX::ThrowIfFailed(
			D3D11CreateDevice(
				nullptr,
				D3D_DRIVER_TYPE_WARP, // Create a WARP device instead of a hardware device.
				0,
				creationFlags,
				featureLevels,
				ARRAYSIZE(featureLevels),
				D3D11_SDK_VERSION,
				&device,
				&m_d3dFeatureLevel,
				&context
				)
			);
	}

	// Store pointers to the Direct3D 11.1 API device and immediate context.
	DX::ThrowIfFailed(
		device.As(&m_d3dDevice)
		);

	DX::ThrowIfFailed(
		context.As(&m_d3dContext)
		);

	// Create the Direct2D device object and a corresponding context.
	ComPtr<IDXGIDevice3> dxgiDevice;
	DX::ThrowIfFailed(
		m_d3dDevice.As(&dxgiDevice)
		);

	DX::ThrowIfFailed(
		m_d2dFactory->CreateDevice(dxgiDevice.Get(), &m_d2dDevice)
		);

	DX::ThrowIfFailed(
		m_d2dDevice->CreateDeviceContext(
			D2D1_DEVICE_CONTEXT_OPTIONS_NONE,
			&m_d2dContext
			)
		);
}

// These resources need to be recreated every time the window size is changed.
void DeviceResources::CreateWindowSizeDependentResources() 
{
	// Clear our previous window size specific context
	ID3D11RenderTargetView* nullViews[] = {nullptr};
	m_d3dContext->OMSetRenderTargets(ARRAYSIZE(nullViews), nullViews, nullptr);
	m_d3dRenderTargetView = nullptr;
	m_d2dContext->SetTarget(nullptr);
	m_d2dTargetBitmap = nullptr;
	m_d3dDepthStencilView = nullptr;
	m_d3dContext->Flush();

	// Store the output bounds so the next time we get a SizeChanged event we can
	// avoid rebuilding everything if the size is identical.
	DisplayInformation^ currentDisplayInformation = DisplayInformation::GetForCurrentView();
	m_outputSize.Width = m_swapChainPanel == nullptr ? m_window->Bounds.Width : static_cast<float>(m_swapChainPanel->ActualWidth);
    m_outputSize.Height = m_swapChainPanel == nullptr ? m_window->Bounds.Height : static_cast<float>(m_swapChainPanel->ActualHeight);

	// Prevent zero size DirectX content from being created
	m_outputSize.Width = m_outputSize.Width > 0 ? m_outputSize.Width : 1;
	m_outputSize.Height = m_outputSize.Height > 0 ? m_outputSize.Height : 1;

	// Calculate the necessary swap chain and render target size in pixels.
	float outputWidthInPixels;
	float outputHeightInPixels;

	if (m_swapChainPanel != nullptr)
	{
		outputWidthInPixels = m_outputSize.Width * m_swapChainPanel->CompositionScaleX;
		outputHeightInPixels = m_outputSize.Height * m_swapChainPanel->CompositionScaleY;
	}
	else
	{
		outputWidthInPixels = DX::ConvertDipsToPixels(m_outputSize.Width, currentDisplayInformation->LogicalDpi);
		outputHeightInPixels = DX::ConvertDipsToPixels(m_outputSize.Height, currentDisplayInformation->LogicalDpi);
	}


	// The width and height of the swap chain must be based on the window's
	// natively-oriented width and height. If the window is not in the native
	// orientation, the dimensions must be reversed.
	m_orientation = currentDisplayInformation->CurrentOrientation;

	DXGI_MODE_ROTATION displayRotation = ComputeDisplayRotation();

	bool swapDimensions = displayRotation == DXGI_MODE_ROTATION_ROTATE90 || displayRotation == DXGI_MODE_ROTATION_ROTATE270;
	m_d3dRenderTargetSize.Width = swapDimensions ? outputHeightInPixels : outputWidthInPixels;
	m_d3dRenderTargetSize.Height = swapDimensions ? outputWidthInPixels : outputHeightInPixels;

	if (m_swapChain)
	{
		// If the swap chain already exists, resize it.
		HRESULT hr = m_swapChain->ResizeBuffers(
			2, // Double-buffered swap chain.
			static_cast<UINT>(m_d3dRenderTargetSize.Width),
			static_cast<UINT>(m_d3dRenderTargetSize.Height),
			DXGI_FORMAT_B8G8R8A8_UNORM,
			0
			);

		if (hr == DXGI_ERROR_DEVICE_REMOVED || hr == DXGI_ERROR_DEVICE_RESET)
		{
			// If the device was removed for any reason, a new device and swap chain will need to be created.
			HandleDeviceLost();

			// Everything is set up now. Do not continue execution of this method. 
			return;
		}
		else
		{
			DX::ThrowIfFailed(hr);
		}
	}
	else
	{
		// Otherwise, create a new one using the same adapter as the existing Direct3D device.
		DXGI_SWAP_CHAIN_DESC1 swapChainDesc = {0};

		swapChainDesc.Width = static_cast<UINT>(m_d3dRenderTargetSize.Width); // Match the size of the window.
		swapChainDesc.Height = static_cast<UINT>(m_d3dRenderTargetSize.Height);
		swapChainDesc.Format = DXGI_FORMAT_B8G8R8A8_UNORM; // This is the most common swap chain format.
		swapChainDesc.Stereo = false;
		swapChainDesc.SampleDesc.Count = 1; // Don't use multi-sampling.
		swapChainDesc.SampleDesc.Quality = 0;
		swapChainDesc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
		swapChainDesc.BufferCount = 2; // Use double-buffering to minimize latency.
		swapChainDesc.SwapEffect = DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL; // All Windows Store apps must use this SwapEffect.
		swapChainDesc.Flags = 0;

		// When using XAML interop, change the Scaling to DXGI_SCALING_STRETCH
		if (m_swapChainPanel == nullptr)
		{
			swapChainDesc.Scaling = DXGI_SCALING_NONE;
		}
		else
		{
			swapChainDesc.Scaling = DXGI_SCALING_STRETCH;
		}

		swapChainDesc.AlphaMode = DXGI_ALPHA_MODE_IGNORE;

		// This sequence obtains the DXGI factory that was used to create the Direct3D device above.
		ComPtr<IDXGIDevice3> dxgiDevice;
		DX::ThrowIfFailed(
			m_d3dDevice.As(&dxgiDevice)
			);

		ComPtr<IDXGIAdapter> dxgiAdapter;
		DX::ThrowIfFailed(
			dxgiDevice->GetAdapter(&dxgiAdapter)
			);

		ComPtr<IDXGIFactory2> dxgiFactory;
		DX::ThrowIfFailed(
			dxgiAdapter->GetParent(
				__uuidof(IDXGIFactory2), 
				&dxgiFactory
				)
			);

		// When using XAML interop, the SwapChain must be created for composition
		if (m_swapChainPanel == nullptr)
		{
			DX::ThrowIfFailed(
				dxgiFactory->CreateSwapChainForCoreWindow(
					m_d3dDevice.Get(),
					reinterpret_cast<IUnknown*>(m_window.Get()),
					&swapChainDesc,
					nullptr,
					&m_swapChain
					)
				);
		}
		else
		{
			DX::ThrowIfFailed(
				dxgiFactory->CreateSwapChainForComposition(
					m_d3dDevice.Get(),
					&swapChainDesc,
					nullptr,
					&m_swapChain
					)
				);

			// Get backing native interface for SwapChainPanel
			ComPtr<ISwapChainPanelNative> panelNative;
			DX::ThrowIfFailed(
				reinterpret_cast<IUnknown*>(m_swapChainPanel)->QueryInterface(IID_PPV_ARGS(&panelNative))
				);

			// Associate swap chain with SwapChainPanel
			DX::ThrowIfFailed(
				panelNative->SetSwapChain(m_swapChain.Get())
				);		
		}

		// Ensure that DXGI does not queue more than one frame at a time. This both reduces latency and
		// ensures that the application will only render after each VSync, minimizing power consumption.
		DX::ThrowIfFailed(
			dxgiDevice->SetMaximumFrameLatency(1)
			);
	}

	// Set the proper orientation for the swap chain, and generate 2D and
	// 3D matrix transformations for rendering to the rotated swap chain.
	// Note the rotation angle for the 2D and 3D transforms are different.
	// This is due to the difference in coordinate spaces.  Additionally,
	// the 3D matrix is specified explicitly to avoid rounding errors.

	switch (displayRotation)
	{
	case DXGI_MODE_ROTATION_IDENTITY:
		m_orientationTransform2D = Matrix3x2F::Identity();
		m_orientationTransform3D = ScreenRotation::Rotation0;
		break;

	case DXGI_MODE_ROTATION_ROTATE90:
		m_orientationTransform2D = 
			Matrix3x2F::Rotation(90.0f) *
            Matrix3x2F::Translation(m_outputSize.Height, 0.0f);
        m_orientationTransform3D = ScreenRotation::Rotation270;
		break;

	case DXGI_MODE_ROTATION_ROTATE180:
		m_orientationTransform2D = 
			Matrix3x2F::Rotation(180.0f) *
			Matrix3x2F::Translation(m_outputSize.Width, m_outputSize.Height);
		m_orientationTransform3D = ScreenRotation::Rotation180;
		break;

	case DXGI_MODE_ROTATION_ROTATE270:
		m_orientationTransform2D = 
			Matrix3x2F::Rotation(270.0f) *
            Matrix3x2F::Translation(0.0f, m_outputSize.Width);
        m_orientationTransform3D = ScreenRotation::Rotation90;
		break;

	default:
		throw ref new Platform::FailureException();
	}

	DX::ThrowIfFailed(
		m_swapChain->SetRotation(displayRotation)
		);

	// Setup inverse scale on the swapchain
	if (m_swapChainPanel != nullptr)
	{
		DXGI_MATRIX_3X2_F inverseScale = { 0 };
		inverseScale._11 = 1.0f / m_swapChainPanel->CompositionScaleX;
		inverseScale._22 = 1.0f / m_swapChainPanel->CompositionScaleY;
		ComPtr<IDXGISwapChain2> spSwapChain2;
		m_swapChain.As<IDXGISwapChain2>(&spSwapChain2);
		spSwapChain2->SetMatrixTransform(&inverseScale);
	}


	// Create a render target view of the swap chain back buffer.
	ComPtr<ID3D11Texture2D> backBuffer;
	DX::ThrowIfFailed(
		m_swapChain->GetBuffer(0, IID_PPV_ARGS(&backBuffer))
		);

	DX::ThrowIfFailed(
		m_d3dDevice->CreateRenderTargetView(
			backBuffer.Get(),
			nullptr,
			&m_d3dRenderTargetView
			)
		);

	// Create a depth stencil view for use with 3D rendering if needed.
	CD3D11_TEXTURE2D_DESC depthStencilDesc(
		DXGI_FORMAT_D24_UNORM_S8_UINT, 
		static_cast<UINT>(m_d3dRenderTargetSize.Width),
		static_cast<UINT>(m_d3dRenderTargetSize.Height),
		1, // This depth stencil view has only one texture.
		1, // Use a single mipmap level.
		D3D11_BIND_DEPTH_STENCIL
		);

	ComPtr<ID3D11Texture2D> depthStencil;
	DX::ThrowIfFailed(
		m_d3dDevice->CreateTexture2D(
			&depthStencilDesc,
			nullptr,
			&depthStencil
			)
		);

	CD3D11_DEPTH_STENCIL_VIEW_DESC depthStencilViewDesc(D3D11_DSV_DIMENSION_TEXTURE2D);
	DX::ThrowIfFailed(
		m_d3dDevice->CreateDepthStencilView(
			depthStencil.Get(),
			&depthStencilViewDesc,
			&m_d3dDepthStencilView
			)
		);
	
	// Set the 3D rendering viewport to target the entire window.
	m_screenViewport = CD3D11_VIEWPORT(
		0.0f,
		0.0f,
		m_d3dRenderTargetSize.Width,
		m_d3dRenderTargetSize.Height
		);

	m_d3dContext->RSSetViewports(1, &m_screenViewport);

	// Create a Direct2D target bitmap associated with the
	// swap chain back buffer and set it as the current target.
	D2D1_BITMAP_PROPERTIES1 bitmapProperties = 
		D2D1::BitmapProperties1(
			D2D1_BITMAP_OPTIONS_TARGET | D2D1_BITMAP_OPTIONS_CANNOT_DRAW,
			D2D1::PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED),
			m_dpi,
			m_dpi
			);

	ComPtr<IDXGISurface2> dxgiBackBuffer;
	DX::ThrowIfFailed(
		m_swapChain->GetBuffer(0, IID_PPV_ARGS(&dxgiBackBuffer))
		);

	DX::ThrowIfFailed(
		m_d2dContext->CreateBitmapFromDxgiSurface(
			dxgiBackBuffer.Get(),
			&bitmapProperties,
			&m_d2dTargetBitmap
			)
		);

	m_d2dContext->SetTarget(m_d2dTargetBitmap.Get());

	// Grayscale text anti-aliasing is recommended for all Windows Store apps.
	m_d2dContext->SetTextAntialiasMode(D2D1_TEXT_ANTIALIAS_MODE_GRAYSCALE);
}

// This method is called when the CoreWindow is created (or re-created)
void DeviceResources::SetWindow(CoreWindow^ window)
{
	m_window = window;
	
	// SetDpi() will call CreateWindowSizeDependentResources()
	// if those resources have not been created yet.
	SetDpi(DisplayInformation::GetForCurrentView()->LogicalDpi);

	UpdateForWindowSizeChange();
}

// This method is called when the XAML control is created (or re-created)
void DeviceResources::SetWindow(CoreWindow^ window, SwapChainPanel^ panel)
{
	m_swapChainPanel = panel;

    SetWindow(window);
}

// This method is called in the event handler for the LogicalDpiChanged event.
void DeviceResources::SetDpi(float dpi)
{
	if (dpi != m_dpi)
	{
		// Save the updated DPI value.
		m_dpi = dpi;
		
		// Update Direct2D's stored DPI.
		m_d2dContext->SetDpi(m_dpi, m_dpi);

		// Often a DPI change implies a window size change. In some cases Windows will issue
		// both a size changed event and a DPI changed event. In this case, the resulting bounds 
		// will not change, and the window resize code will only be executed once.
	    
		UpdateForWindowSizeChange();
	}
}

// This method is called in the event handler for the SizeChanged event.
void DeviceResources::UpdateForWindowSizeChange()
{
	DisplayInformation^ currentDisplayInformation = DisplayInformation::GetForCurrentView();

	if (m_swapChainPanel == nullptr && (
            m_window->Bounds.Width  != m_outputSize.Width ||
            m_window->Bounds.Height != m_outputSize.Height
        ) || m_swapChainPanel != nullptr && (
            m_swapChainPanel->ActualWidth != m_outputSize.Width ||
            m_swapChainPanel->ActualHeight != m_outputSize.Height
        ) || m_orientation != currentDisplayInformation->CurrentOrientation)

	{
		CreateWindowSizeDependentResources();
	}
}

// This method is called in the event handler for the DisplayContentsInvalidated event.
void DeviceResources::ValidateDevice()
{
	// The D3D Device is no longer valid if the default adapter changes or if 
	// the device has been removed. 
 
	// First, get the information for the adapter related to the current device. 

	ComPtr<IDXGIDevice3> dxgiDevice;
	DX::ThrowIfFailed(m_d3dDevice.As(&dxgiDevice));

	ComPtr<IDXGIAdapter> deviceAdapter;
	DX::ThrowIfFailed(dxgiDevice->GetAdapter(&deviceAdapter));

	DXGI_ADAPTER_DESC adapterDesc;
	DX::ThrowIfFailed(deviceAdapter->GetDesc(&adapterDesc));

	// Next, get the information for the default adapter. 

	ComPtr<IDXGIFactory2> dxgiFactory;
	DX::ThrowIfFailed(CreateDXGIFactory1(IID_PPV_ARGS(&dxgiFactory)));

	ComPtr<IDXGIAdapter1> currentAdapter;
	DX::ThrowIfFailed(dxgiFactory->EnumAdapters1(0, &currentAdapter));

	DXGI_ADAPTER_DESC currentDesc;
	DX::ThrowIfFailed(currentAdapter->GetDesc(&currentDesc));

	// If the adapter LUIDs don't match, or if the device reports that it has been removed, 
	// a new D3D device must be created. 

	if (adapterDesc.AdapterLuid.LowPart != currentDesc.AdapterLuid.LowPart ||
		adapterDesc.AdapterLuid.HighPart != currentDesc.AdapterLuid.HighPart ||
		FAILED(m_d3dDevice->GetDeviceRemovedReason()))
	{
		// Release references to resources related to the old device. 
		dxgiDevice = nullptr;
		deviceAdapter = nullptr;

		// Create a new device and swap chain. 
		HandleDeviceLost();
	}
}

// Recreate all device resources and set them back to the current state.
void DeviceResources::HandleDeviceLost()
{
		// Reset these member variables to ensure that SetDpi recreates all resources.
	float dpi = m_dpi;
	m_dpi = -1.0f;
	m_outputSize.Width = 0;
    m_outputSize.Height = 0;
	m_swapChain = nullptr;

	if (m_deviceNotify != nullptr)
	{
		m_deviceNotify->OnDeviceLost();
	}

	CreateDeviceResources();
	SetDpi(dpi);

	if (m_deviceNotify != nullptr)
	{
		m_deviceNotify->OnDeviceRecreated();
	}
}

// Register our DeviceNotify to be informed on device lost and creation
void DeviceResources::RegisterDeviceNotify(IDeviceNotify* deviceNotify)
{
	m_deviceNotify = deviceNotify;
}

// Call this method when the app suspends to hint to the driver that the app is entering an idle state
// and that temporary buffers can be reclaimed for use by other apps.
void DeviceResources::Trim()
{
	ComPtr<IDXGIDevice3> dxgiDevice;
	m_d3dDevice.As(&dxgiDevice);

	dxgiDevice->Trim();
}

// Present the contents of the swap chain to the screen.
void DeviceResources::Present() 
{
	// The first argument instructs DXGI to block until VSync, putting the application
	// to sleep until the next VSync. This ensures we don't waste any cycles rendering
	// frames that will never be displayed to the screen.
	HRESULT hr = m_swapChain->Present(1, 0);

	// Discard the contents of the render target.
	// This is a valid operation only when the existing contents will be entirely
	// overwritten. If dirty or scroll rects are used, this call should be removed.
	m_d3dContext->DiscardView(m_d3dRenderTargetView.Get());

	// Discard the contents of the depth stencil.
	m_d3dContext->DiscardView(m_d3dDepthStencilView.Get());

	// If the device was removed either by a disconnect or a driver upgrade, we 
	// must recreate all device resources.
	if (hr == DXGI_ERROR_DEVICE_REMOVED || hr == DXGI_ERROR_DEVICE_RESET)
	{
		HandleDeviceLost();
	}
	else
	{
		DX::ThrowIfFailed(hr);
	}
}

// This method determines the rotation between the display device's native Orientation and the
// current display orientation.
DXGI_MODE_ROTATION DeviceResources::ComputeDisplayRotation()
{
    DXGI_MODE_ROTATION rotation = DXGI_MODE_ROTATION_UNSPECIFIED;

    // Note: NativeOrientation can only be Landscape or Portrait even though
    // the DisplayOrientations enum has other values.
    auto displayInformation = DisplayInformation::GetForCurrentView();

    switch (displayInformation->NativeOrientation)
    {
    case DisplayOrientations::Landscape:
        switch (displayInformation->CurrentOrientation)
        {
        case DisplayOrientations::Landscape:
            rotation = DXGI_MODE_ROTATION_IDENTITY;
            break;

        case DisplayOrientations::Portrait:
            rotation = DXGI_MODE_ROTATION_ROTATE90;
            break;

        case DisplayOrientations::LandscapeFlipped:
            rotation = DXGI_MODE_ROTATION_ROTATE180;
            break;

        case DisplayOrientations::PortraitFlipped:
            rotation = DXGI_MODE_ROTATION_ROTATE270;
            break;
        }
        break;

    case DisplayOrientations::Portrait:
        switch (displayInformation->CurrentOrientation)
        {
        case DisplayOrientations::Landscape:
            rotation = DXGI_MODE_ROTATION_ROTATE270;
            break;

        case DisplayOrientations::Portrait:
            rotation = DXGI_MODE_ROTATION_IDENTITY;
            break;

        case DisplayOrientations::LandscapeFlipped:
            rotation = DXGI_MODE_ROTATION_ROTATE90;
            break;

        case DisplayOrientations::PortraitFlipped:
            rotation = DXGI_MODE_ROTATION_ROTATE180;
            break;
        }
        break;
    }
    return rotation;
}