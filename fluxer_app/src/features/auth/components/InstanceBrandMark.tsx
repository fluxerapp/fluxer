// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import FluxerLogoAsset from '@app/media/images/fluxer-logo-color.svg?react';
import {GlobeIcon} from '@phosphor-icons/react';
import type React from 'react';
import {useState} from 'react';

interface InstanceBrandMarkProps {
	readonly isOfficial: boolean;
	readonly iconUrl: string | null;
	readonly size: number;
	readonly globeWeight?: 'regular' | 'bold';
	readonly 'data-flx'?: string;
}

function InstanceBrandImage({
	url,
	size,
	fallback,
}: {
	readonly url: string;
	readonly size: number;
	readonly fallback: React.ReactElement;
}): React.ReactElement {
	const [failed, setFailed] = useState(false);
	if (failed) {
		return fallback;
	}
	return (
		<img
			src={url}
			alt=""
			width={size}
			height={size}
			draggable={false}
			decoding="async"
			referrerPolicy="no-referrer"
			onError={() => setFailed(true)}
			data-flx="auth.components.instance-brand-mark.instance-brand-image.image"
		/>
	);
}

export function InstanceBrandMark({
	isOfficial,
	iconUrl,
	size,
	globeWeight = 'regular',
	'data-flx': dataFlx,
}: InstanceBrandMarkProps): React.ReactElement {
	if (isOfficial) {
		return <FluxerLogoAsset aria-hidden="true" data-flx={dataFlx} />;
	}
	const globe = <GlobeIcon size={remFromPx(size)} weight={globeWeight} aria-hidden="true" data-flx={dataFlx} />;
	if (iconUrl == null) {
		return globe;
	}
	return (
		<InstanceBrandImage
			key={iconUrl}
			url={iconUrl}
			size={size}
			fallback={globe}
			data-flx="auth.instance-brand-mark.instance-brand-image"
		/>
	);
}
