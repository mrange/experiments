#pragma once

#include "DeviceResources.h"

// Renders Direct2D and 3D content on the screen.
namespace RaytraceApp
{
    class RaytraceAppMain : public IDeviceNotify
    {
    public:
        RaytraceAppMain(const std::shared_ptr<DeviceResources>& deviceResources);
        ~RaytraceAppMain();
        void CreateWindowSizeDependentResources();
        void Update();
        bool Render();

        // IDeviceNotify
        virtual void OnDeviceLost();
        virtual void OnDeviceRecreated();

        void PointerWheelChanged(Windows::Foundation::Point const & p, int delta);
        void PointerMoved(Windows::Foundation::Point const & p);
        void KeyUp(Windows::System::VirtualKey vk);

    private:
        struct Impl;

        std::unique_ptr<Impl> m_impl;

    };
}