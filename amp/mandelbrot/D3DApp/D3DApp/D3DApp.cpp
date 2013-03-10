#include "pch.h"
#include "D3DApp.h"
#include "BasicTimer.h"

#include "CubeRenderer.h"
#include "MandelBrotRenderer.h"

using namespace Windows::ApplicationModel;
using namespace Windows::ApplicationModel::Core;
using namespace Windows::ApplicationModel::Activation;
using namespace Windows::UI::Core;
using namespace Windows::System;
using namespace Windows::Foundation;
using namespace Windows::Graphics::Display;
using namespace concurrency;

D3DApp::D3DApp() :
	m_windowClosed(false),
	m_windowVisible(true)
{
}

void D3DApp::Initialize(CoreApplicationView^ applicationView)
{
	applicationView->Activated +=
        ref new TypedEventHandler<CoreApplicationView^, IActivatedEventArgs^>(this, &D3DApp::OnActivated);

	CoreApplication::Suspending +=
        ref new EventHandler<SuspendingEventArgs^>(this, &D3DApp::OnSuspending);

	CoreApplication::Resuming +=
        ref new EventHandler<Platform::Object^>(this, &D3DApp::OnResuming);

	m_renderer = ref new MandelBrotRenderer();
}

void D3DApp::SetWindow(CoreWindow^ window)
{
	window->SizeChanged += 
        ref new TypedEventHandler<CoreWindow^, WindowSizeChangedEventArgs^>(this, &D3DApp::OnWindowSizeChanged);

	window->VisibilityChanged +=
		ref new TypedEventHandler<CoreWindow^, VisibilityChangedEventArgs^>(this, &D3DApp::OnVisibilityChanged);

	window->Closed += 
        ref new TypedEventHandler<CoreWindow^, CoreWindowEventArgs^>(this, &D3DApp::OnWindowClosed);

	window->PointerCursor = ref new CoreCursor(CoreCursorType::Arrow, 0);

	window->PointerPressed +=
		ref new TypedEventHandler<CoreWindow^, PointerEventArgs^>(this, &D3DApp::OnPointerPressed);

	window->PointerMoved +=
		ref new TypedEventHandler<CoreWindow^, PointerEventArgs^>(this, &D3DApp::OnPointerMoved);

	m_renderer->Initialize(CoreWindow::GetForCurrentThread());
}

void D3DApp::Load(Platform::String^ entryPoint)
{
}

void D3DApp::Run()
{
	BasicTimer^ timer = ref new BasicTimer();

	while (!m_windowClosed)
	{
		if (m_windowVisible)
		{
			timer->Update();
			CoreWindow::GetForCurrentThread()->Dispatcher->ProcessEvents(CoreProcessEventsOption::ProcessAllIfPresent);
			m_renderer->Update(timer->Total, timer->Delta);
            m_renderer->Clear ();
            m_renderer->Render();
			m_renderer->Present(); // This call is synchronized to the display frame rate.
		}
		else
		{
			CoreWindow::GetForCurrentThread()->Dispatcher->ProcessEvents(CoreProcessEventsOption::ProcessOneAndAllPending);
		}
	}
}

void D3DApp::Uninitialize()
{
}

void D3DApp::OnWindowSizeChanged(CoreWindow^ sender, WindowSizeChangedEventArgs^ args)
{
	m_renderer->UpdateForWindowSizeChange();
}

void D3DApp::OnVisibilityChanged(CoreWindow^ sender, VisibilityChangedEventArgs^ args)
{
	m_windowVisible = args->Visible;
}

void D3DApp::OnWindowClosed(CoreWindow^ sender, CoreWindowEventArgs^ args)
{
	m_windowClosed = true;
}

void D3DApp::OnPointerPressed(CoreWindow^ sender, PointerEventArgs^ args)
{
	// Insert your code here.
}

void D3DApp::OnPointerMoved(CoreWindow^ sender, PointerEventArgs^ args)
{
	// Insert your code here.
}

void D3DApp::OnActivated(CoreApplicationView^ applicationView, IActivatedEventArgs^ args)
{
	CoreWindow::GetForCurrentThread()->Activate();
}

void D3DApp::OnSuspending(Platform::Object^ sender, SuspendingEventArgs^ args)
{
	// Save app state asynchronously after requesting a deferral. Holding a deferral
	// indicates that the application is busy performing suspending operations. Be
	// aware that a deferral may not be held indefinitely. After about five seconds,
	// the app will be forced to exit.
	SuspendingDeferral^ deferral = args->SuspendingOperation->GetDeferral();

	create_task([this, deferral]()
	{
		// Insert your code here.

		deferral->Complete();
	});
}
 
void D3DApp::OnResuming(Platform::Object^ sender, Platform::Object^ args)
{
	// Restore any data or state that was unloaded on suspend. By default, data
	// and state are persisted when resuming from suspend. Note that this event
	// does not occur if the app was previously terminated.
}

IFrameworkView^ Direct3DApplicationSource::CreateView()
{
    return ref new D3DApp();
}

[Platform::MTAThread]
int main(Platform::Array<Platform::String^>^)
{
	auto direct3DApplicationSource = ref new Direct3DApplicationSource();
	CoreApplication::Run(direct3DApplicationSource);
	return 0;
}
