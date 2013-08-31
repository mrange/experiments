#pragma once

#include "DeviceResources.h"

// Renders Direct2D and 3D content on the screen.
namespace MandelbrotApp
{
    class MandelbrotAppMain : public IDeviceNotify
    {
    public:
        MandelbrotAppMain(const std::shared_ptr<DeviceResources>& deviceResources);
        ~MandelbrotAppMain();
        void CreateWindowSizeDependentResources();
        void Update();
        bool Render();

        // IDeviceNotify
        virtual void OnDeviceLost();
        virtual void OnDeviceRecreated();

        void PointerWheelChanged(Windows::Foundation::Point const & p, int delta);
        void PointerMoved(Windows::Foundation::Point const & p);

    private:
        struct Impl;

        std::unique_ptr<Impl> m_impl;

    };
}