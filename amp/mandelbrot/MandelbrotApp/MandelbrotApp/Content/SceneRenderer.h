#pragma once

#include "..\DeviceResources.h"
#include "ShaderStructures.h"
#include "..\Common\StepTimer.h"

namespace MandelbrotApp
{
	// This sample renderer instantiates a basic rendering pipeline.
	struct SceneRenderer
	{
		SceneRenderer(const std::shared_ptr<DeviceResources>& deviceResources);
        ~SceneRenderer();

		void CreateDeviceDependentResources();
		void CreateWindowSizeDependentResources();
		void ReleaseDeviceDependentResources();
		void Update(DX::StepTimer const& timer);
		void Render();

        void PointerWheelChanged(Windows::Foundation::Point const & p, int delta);
        void PointerMoved(Windows::Foundation::Point const & p);

	private:

        struct Impl;

        std::unique_ptr<Impl> m_impl;

	};
}

