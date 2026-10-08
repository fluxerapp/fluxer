// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveAppShellBranding} from '@app/features/app/state/AppShellBranding';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {type BrandSvgProps, getDataFlx, getImageSizingProps} from '@app/features/ui/components/icons/BrandImageUtils';
import FluxerLogoAsset from '@app/media/images/fluxer-logo-color.svg?react';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const APPLICATION_LOGO_DESCRIPTOR = msg({
	message: '{productName} application logo',
	comment: 'Accessible label for the application logo.',
});

export const FluxerLogo = observer((props: BrandSvgProps) => {
	const {i18n} = useLingui();
	const branding = resolveAppShellBranding(RuntimeConfig.getSnapshotOrNull());
	const ariaLabel = i18n._(APPLICATION_LOGO_DESCRIPTOR, {productName: branding.productName});
	if (branding.logoUrl !== null) {
		return (
			<img
				{...getImageSizingProps(props)}
				src={branding.logoUrl}
				alt={ariaLabel}
				data-flx={getDataFlx(props, 'ui.icons.fluxer-logo.img')}
			/>
		);
	}
	return <FluxerLogoAsset role="img" aria-label={ariaLabel} data-flx="ui.icons.fluxer-logo.img" {...props} />;
});
