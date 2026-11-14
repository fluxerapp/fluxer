// SPDX-License-Identifier: AGPL-3.0-or-later

import {DEFAULT_APP_SHELL_BRANDING, resolveAppShellBranding} from '@app/features/app/state/AppShellBranding';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {type BrandSvgProps, getDataFlx, getImageSizingProps} from '@app/features/ui/components/icons/BrandImageUtils';
import FluxerWordmarkMonochromeAsset from '@app/media/images/fluxer-logo-wordmark-monochrome.svg?react';
import FluxerWordmarkAsset from '@app/media/images/fluxer-wordmark.svg?react';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useState} from 'react';

const APPLICATION_WORDMARK_DESCRIPTOR = msg({
	message: '{productName} wordmark',
	comment: 'Accessible label for the application wordmark.',
});

export interface WordmarkBranding {
	readonly productName: string;
	readonly wordmarkUrl: string | null;
}

interface FluxerWordmarkProps extends BrandSvgProps {
	variant?: 'default' | 'monochrome';
	branding?: WordmarkBranding;
	onImageError?: () => void;
}

export const FluxerWordmark = observer(
	({variant = 'default', branding, onImageError, ...props}: FluxerWordmarkProps) => {
		const {i18n} = useLingui();
		const [failedUrl, setFailedUrl] = useState<string | null>(null);
		const resolved = branding ?? resolveAppShellBranding(RuntimeConfig.getSnapshotOrNull());
		const productName = resolved.productName;
		const wordmarkUrl = resolved.wordmarkUrl;
		const ariaLabel = i18n._(APPLICATION_WORDMARK_DESCRIPTOR, {productName});
		if (wordmarkUrl !== null && wordmarkUrl !== failedUrl) {
			return (
				<img
					{...getImageSizingProps(props)}
					src={wordmarkUrl}
					alt={ariaLabel}
					draggable={false}
					decoding="async"
					referrerPolicy="no-referrer"
					onError={() => {
						setFailedUrl(wordmarkUrl);
						onImageError?.();
					}}
					data-flx={getDataFlx(props, 'ui.icons.fluxer-wordmark.img')}
				/>
			);
		}
		if (productName !== DEFAULT_APP_SHELL_BRANDING.productName) {
			const style: React.CSSProperties = {
				alignItems: 'center',
				display: 'inline-flex',
				fontWeight: 800,
				lineHeight: 1,
				...(props.style as React.CSSProperties | undefined),
			};
			return (
				<span
					className={props.className}
					style={style}
					role="img"
					dir="auto"
					aria-label={ariaLabel}
					data-flx={getDataFlx(props, 'ui.icons.fluxer-wordmark.text')}
				>
					{productName}
				</span>
			);
		}
		const Asset = variant === 'monochrome' ? FluxerWordmarkMonochromeAsset : FluxerWordmarkAsset;
		return <Asset role="img" aria-label={ariaLabel} data-flx="ui.icons.fluxer-wordmark.img" {...props} />;
	},
);
