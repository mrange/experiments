#include "pch.h"
#include "App.h"

#include <ppltasks.h>    // For create_task

using namespace MandelbrotApp;

using namespace concurrency;
using namespace Windows::ApplicationModel;
using namespace Windows::ApplicationModel::Core;
using namespace Windows::ApplicationModel::Activation;
using namespace Windows::UI::Core;
using namespace Windows::UI::Input;
using namespace Windows::System;
using namespace Windows::Foundation;
using namespace Windows::Graphics::Display;


namespace
{
    ref class App sealed : public Windows::ApplicationModel::Core::IFrameworkView
    {
    public:
        App()
            :   m_windowClosed(false)
            ,   m_windowVisible(true)
        {

        }

        virtual ~App()
        {

        }

        // The first method called when the IFrameworkView is being created.
        virtual void Initialize(CoreApplicationView^ applicationView)
        {
            // Register event handlers for app lifecycle. This example includes Activated, so that we
            // can make the CoreWindow active and start rendering on the window.
            applicationView->Activated +=
                ref new TypedEventHandler<CoreApplicationView^, IActivatedEventArgs^>(this, &App::OnActivated);

            CoreApplication::Suspending +=
                ref new EventHandler<SuspendingEventArgs^>(this, &App::OnSuspending);

            CoreApplication::Resuming +=
                ref new EventHandler<Platform::Object^>(this, &App::OnResuming);

            // At this point we have access to the device. 
            // We can create the device-dependent resources.
            m_deviceResources = std::make_shared<DeviceResources>();
        }

        // Called when the CoreWindow object is created (or re-created).
        virtual void SetWindow(CoreWindow^ window)
        {
            window->SizeChanged += 
                ref new TypedEventHandler<CoreWindow^, WindowSizeChangedEventArgs^>(this, &App::OnWindowSizeChanged);

            window->VisibilityChanged +=
                ref new TypedEventHandler<CoreWindow^, VisibilityChangedEventArgs^>(this, &App::OnVisibilityChanged);

            window->Closed += 
                ref new TypedEventHandler<CoreWindow^, CoreWindowEventArgs^>(this, &App::OnWindowClosed);

            DisplayInformation^ currentDisplayInformation = DisplayInformation::GetForCurrentView();

            currentDisplayInformation->DpiChanged +=
                ref new TypedEventHandler<DisplayInformation^, Object^>(this, &App::OnDpiChanged);

            currentDisplayInformation->OrientationChanged +=
                ref new TypedEventHandler<DisplayInformation^, Object^>(this, &App::OnOrientationChanged);

            DisplayInformation::DisplayContentsInvalidated +=
                ref new TypedEventHandler<DisplayInformation^, Object^>(this, &App::OnDisplayContentsInvalidated);

            // Disable all pointer visual feedback for better performance when touching.
            auto pointerVisualizationSettings = PointerVisualizationSettings::GetForCurrentView();
            pointerVisualizationSettings->IsContactFeedbackEnabled = false; 
            pointerVisualizationSettings->IsBarrelButtonFeedbackEnabled = false;

            window->PointerMoved +=ref new TypedEventHandler<CoreWindow^, PointerEventArgs^>(this, &App::OnPointerMoved); 
            window->PointerWheelChanged +=ref new TypedEventHandler<CoreWindow^, PointerEventArgs^>(this, &App::OnPointerWheelChanged); 
            window->KeyUp += ref new TypedEventHandler<CoreWindow^, KeyEventArgs^>(this, &App::OnKeyUp); 

            m_deviceResources->SetWindow(window);
        }

        // Initializes scene resources, or loads a previously saved app state.
        virtual void Load(Platform::String^ entryPoint)
        {
                m_main = std::unique_ptr<MandelbrotAppMain>(new MandelbrotAppMain(m_deviceResources));
        }

        // This method is called after the window becomes active.
        virtual void Run()
        {
            while (!m_windowClosed)
            {
                if (m_windowVisible)
                {
                    CoreWindow::GetForCurrentThread()->Dispatcher->ProcessEvents(CoreProcessEventsOption::ProcessAllIfPresent);

                    m_main->Update();

                    if (m_main->Render())
                    {
                        m_deviceResources->Present();
                    }
                }
                else
                {
                    CoreWindow::GetForCurrentThread()->Dispatcher->ProcessEvents(CoreProcessEventsOption::ProcessOneAndAllPending);
                }
            }
        }

        // Required for IFrameworkView.
        // Terminate events do not cause Uninitialize to be called. It will be called if your IFrameworkView
        // class is torn down while the app is in the foreground.
        virtual void Uninitialize()
        {
        }

        // Application lifecycle event handlers.

        virtual void OnActivated(CoreApplicationView^ applicationView, IActivatedEventArgs^ args)
        {
            // Run() won't start until the CoreWindow is activated.
            CoreWindow::GetForCurrentThread()->Activate();
        }

        virtual void OnSuspending(Platform::Object^ sender, SuspendingEventArgs^ args)
        {
            // Save app state asynchronously after requesting a deferral. Holding a deferral
            // indicates that the application is busy performing suspending operations. Be
            // aware that a deferral may not be held indefinitely. After about five seconds,
            // the app will be forced to exit.
            SuspendingDeferral^ deferral = args->SuspendingOperation->GetDeferral();

            create_task([this, deferral]()
            {
                m_deviceResources->Trim();

                // Insert your code here.

                deferral->Complete();
            });
        }

        virtual void OnResuming(Platform::Object^ sender, Platform::Object^ args)
        {
            // Restore any data or state that was unloaded on suspend. By default, data
            // and state are persisted when resuming from suspend. Note that this event
            // does not occur if the app was previously terminated.

            // Insert your code here.
        }

        // Window event handlers.

        virtual void OnWindowSizeChanged(CoreWindow^ sender, WindowSizeChangedEventArgs^ args)
        {
            m_deviceResources->UpdateForWindowSizeChange();
            m_main->CreateWindowSizeDependentResources();
        }

        virtual void OnVisibilityChanged(CoreWindow^ sender, VisibilityChangedEventArgs^ args)
        {
            m_windowVisible = args->Visible;
        }

        virtual void OnWindowClosed(CoreWindow^ sender, CoreWindowEventArgs^ args)
        {
            m_windowClosed = true;
        }

        // Display properties event handlers.

        virtual void OnDpiChanged(DisplayInformation^ sender, Object^ args)
        {
            m_deviceResources->SetDpi(sender->LogicalDpi);
        }

        virtual void OnOrientationChanged(DisplayInformation^ sender, Object^ args)
        {
            m_deviceResources->UpdateForWindowSizeChange();
            m_main->CreateWindowSizeDependentResources();
        }

        virtual void OnDisplayContentsInvalidated(DisplayInformation^ sender, Object^ args)
        {
            m_deviceResources->ValidateDevice();
        }

        virtual void OnPointerWheelChanged(_In_ CoreWindow^ sender, _In_ PointerEventArgs^ args)
        {
            m_main->PointerWheelChanged(args->CurrentPoint->Position, args->CurrentPoint->Properties->MouseWheelDelta);
        }

        virtual void OnPointerMoved(_In_ CoreWindow^ sender, _In_ PointerEventArgs^ args)
        {
            m_main->PointerMoved(args->CurrentPoint->Position);
        }

        virtual void OnKeyUp(_In_ CoreWindow^ sender, _In_ KeyEventArgs^ args)
        {
        }

    private:
        std::shared_ptr<DeviceResources> m_deviceResources;
        std::unique_ptr<MandelbrotAppMain> m_main;
        bool m_windowClosed;
        bool m_windowVisible;
    };

    ref class Direct3DApplicationSource sealed : Windows::ApplicationModel::Core::IFrameworkViewSource
    {
    public:
        virtual IFrameworkView^ CreateView()
        {
            return ref new App();
        }
    };

}

// The main function is only used to initialize our IFrameworkView class.
[Platform::MTAThread]
int main(Platform::Array<Platform::String^>^)
{
    auto direct3DApplicationSource = ref new Direct3DApplicationSource();
    CoreApplication::Run(direct3DApplicationSource);
    return 0;
}

