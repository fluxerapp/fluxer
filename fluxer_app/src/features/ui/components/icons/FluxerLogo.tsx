// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import FluxerLogoAsset from '@app/media/images/fluxer-logo-color.svg?react';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {type BrandSvgProps, getDataFlx, getImageSizingProps} from './BrandImageUtils';

const APPLICATION_LOGO_DESCRIPTOR = msg({
	message: '{productName} application logo',
	comment: 'Accessible label for the application logo.',
});

export const FluxerLogo = observer((props: BrandSvgProps) => {
	const {i18n} = useLingui();
	const ariaLabel = i18n._(APPLICATION_LOGO_DESCRIPTOR, {productName: RuntimeConfig.productName});
	if (RuntimeConfig.logoUrl) {
		return (
			<img
				{...getImageSizingProps(props)}
				src={RuntimeConfig.logoUrl}
				alt={ariaLabel}
				data-flx={getDataFlx(props, 'ui.icons.fluxer-logo.img')}
			/>
		);
	}
	return <FluxerLogoAsset role="img" aria-label={ariaLabel} data-flx="ui.icons.fluxer-logo.img" {...props} />;
});
