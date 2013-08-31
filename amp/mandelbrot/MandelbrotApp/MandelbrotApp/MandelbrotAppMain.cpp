#include "pch.h"
#include "MandelbrotAppMain.h"

#include <DirectXColors.h>            // For named colors    
#include "Common\DirectXHelper.h"    // For ThrowIfFailed

#include "Common\StepTimer.h"
#include "Content\SceneRenderer.h"

using namespace MandelbrotApp;

namespace
{
}

struct MandelbrotAppMain::Impl
{
    Impl(const std::shared_ptr<DeviceResources>& deviceResources)
        :   m_deviceResources(deviceResources)
    {
        // TODO: Replace this with your app content initialization.
        m_sceneRenderer = std::unique_ptr<SceneRenderer>(std::make_unique<SceneRenderer>(m_deviceResources));
        m_sceneRenderer->CreateWindowSizeDependentResources();

    }

    ~Impl()
    {
        // Deregister from device notification
        m_deviceResources->RegisterDeviceNotify(nullptr);
    }

    void CreateWindowSizeDependentResources()
    {
        // TODO: Replace this with the size-dependent initialization of your app content.
        m_sceneRenderer->CreateWindowSizeDependentResources();
    }

    void Update()
    {
        // Update scene objects.
        m_timer.Tick([&]()
        {
            // TODO: Replace this with your app content update functions.
            m_sceneRenderer->Update(m_timer);
        });
    }

    bool Render()
    {
        // Don't try to render anything before the first Update.
        if (m_timer.GetFrameCount() == 0)
        {
            return false;
        }

        auto context = m_deviceResources->GetD3DDeviceContext();

        // Reset the viewport to target the whole screen.
        auto viewport = m_deviceResources->GetScreenViewport();
        context->RSSetViewports(1, &viewport);

        // Reset render targets to the screen.
        ID3D11RenderTargetView *const targets[1] = { m_deviceResources->GetBackBufferRenderTargetView() };
        context->OMSetRenderTargets(1, targets, m_deviceResources->GetDepthStencilView());

        // Clear the back buffer and depth stencil view.
        context->ClearRenderTargetView(m_deviceResources->GetBackBufferRenderTargetView(), DirectX::Colors::Black);
        context->ClearDepthStencilView(m_deviceResources->GetDepthStencilView(), D3D11_CLEAR_DEPTH | D3D11_CLEAR_STENCIL, 1.0f, 0);

        // Render the scene objects.
        // TODO: Replace this with your app content rendering functions.
        m_sceneRenderer->Render();

        return true;
    }

    void OnDeviceLost()
    {
        m_sceneRenderer->ReleaseDeviceDependentResources();
    }
    void OnDeviceRecreated()
    {
        m_sceneRenderer->CreateDeviceDependentResources();
        CreateWindowSizeDependentResources();
    }

    void PointerWheelChanged(Windows::Foundation::Point const & p, int delta)
    {
        m_sceneRenderer->PointerWheelChanged(p, delta);
    }

    void PointerMoved(Windows::Foundation::Point const & p)
    {
        m_sceneRenderer->PointerMoved(p);
    }

    // Cached pointer to device resources.
    std::shared_ptr<DeviceResources> m_deviceResources;

    // TODO: Replace with your own content.
    std::unique_ptr<SceneRenderer> m_sceneRenderer;

    // Rendering loop timer.
    DX::StepTimer m_timer;
};

MandelbrotAppMain::MandelbrotAppMain(const std::shared_ptr<DeviceResources>& deviceResources)
    : m_impl(std::make_unique<Impl> (deviceResources))
{
    // TODO: Change timer settings if you want something other than the default variable timestep mode.
    // eg. for 60 FPS fixed timestep update logic, call:
    /*
    m_timer.SetFixedTimeStep(true);
    m_timer.SetTargetElapsedSeconds(1.0 / 60);
    */
    m_impl->m_deviceResources->RegisterDeviceNotify(this);
}

MandelbrotAppMain::~MandelbrotAppMain()
{

}

void MandelbrotAppMain::CreateWindowSizeDependentResources()
{
    m_impl->CreateWindowSizeDependentResources();
}

void MandelbrotAppMain::Update()
{
    m_impl->Update();
}

bool MandelbrotAppMain::Render()
{
    return m_impl->Render();
}

// Notifies renderers that device resources need to be released.
void MandelbrotAppMain::OnDeviceLost()
{
    m_impl->OnDeviceLost();
}

// Notifies renderers that device resources may now be recreated.
void MandelbrotAppMain::OnDeviceRecreated()
{
    m_impl->OnDeviceRecreated();
}

void MandelbrotAppMain::PointerWheelChanged(Windows::Foundation::Point const & p, int delta)
{
    m_impl->PointerWheelChanged(p, delta);
}

void MandelbrotAppMain::PointerMoved(Windows::Foundation::Point const & p)
{
    m_impl->PointerMoved(p);
}
