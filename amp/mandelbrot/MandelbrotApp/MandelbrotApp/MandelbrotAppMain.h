#pragma once

#include "Common\StepTimer.h"
#include "DeviceResources.h"
#include "Content\Sample3DSceneRenderer.h"
#include "Content\SampleFpsTextRenderer.h"

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

        void PointerPressed(Windows::Foundation::Point const & p);
        void PointerMoved(Windows::Foundation::Point const & p);

	private:
		// Cached pointer to device resources.
		std::shared_ptr<DeviceResources> m_deviceResources;

		// TODO: Replace with your own content.
		std::unique_ptr<Sample3DSceneRenderer> m_sceneRenderer;
		std::unique_ptr<SampleFpsTextRenderer> m_fpsTextRenderer;

		// Rendering loop timer.
		DX::StepTimer m_timer;
	};
}