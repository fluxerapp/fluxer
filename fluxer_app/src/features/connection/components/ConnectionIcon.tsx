// SPDX-License-Identifier: AGPL-3.0-or-later

import {BLUESKY_PROVIDER_NAME} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/connection/components/ConnectionIcon.module.css';
import {DOMAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {BlueskyIcon} from '@app/features/ui/components/icons/BlueskyIcon';
import {type ConnectionType, ConnectionTypes} from '@fluxer/constants/src/ConnectionConstants';
import type {InstanceConnectionProvider} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {GlobeSimpleIcon, LinkBreakIcon, PlugsConnectedIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

export const UNAVAILABLE_CONNECTION_DESCRIPTOR = msg({
	message: 'No longer available',
	comment:
		'Label for a linked account whose provider this instance removed or turned off. The account is hidden from the profile.',
});

interface ConnectionKind {
	type: ConnectionType;
	providerId: string | null | undefined;
}

function findConnectionProvider(connection: ConnectionKind): InstanceConnectionProvider | null {
	if (connection.type !== ConnectionTypes.OAUTH || !connection.providerId) return null;
	return RuntimeConfig.connectionProviders.find((provider) => provider.id === connection.providerId) ?? null;
}

export function isConnectionUnavailable(connection: ConnectionKind): boolean {
	return connection.type === ConnectionTypes.OAUTH && findConnectionProvider(connection) === null;
}

export function connectionLabel(i18n: I18n, connection: ConnectionKind): string {
	switch (connection.type) {
		case ConnectionTypes.BLUESKY:
			return BLUESKY_PROVIDER_NAME;
		case ConnectionTypes.DOMAIN:
			return i18n._(DOMAIN_DESCRIPTOR);
		case ConnectionTypes.OAUTH:
			return findConnectionProvider(connection)?.name ?? i18n._(UNAVAILABLE_CONNECTION_DESCRIPTOR);
	}
}

export const ProviderIcon = ({provider, size}: {provider: InstanceConnectionProvider; size: number}) =>
	provider.icon_url ? (
		<img
			src={provider.icon_url}
			alt=""
			width={size}
			height={size}
			className={styles.providerIcon}
			style={{width: remFromPx(size), height: remFromPx(size)}}
			draggable={false}
			data-flx="connection.connection-icon.provider-icon.provider-icon"
		/>
	) : (
		<PlugsConnectedIcon
			size={remFromPx(size)}
			className={styles.glyph}
			data-flx="connection.connection-icon.provider-icon.glyph"
		/>
	);

export const ConnectionIcon = observer(({connection, size}: {connection: ConnectionKind; size: number}) => {
	switch (connection.type) {
		case ConnectionTypes.BLUESKY:
			return <BlueskyIcon size={size} data-flx="connection.connection-icon.bluesky-icon" />;
		case ConnectionTypes.DOMAIN:
			return (
				<GlobeSimpleIcon size={remFromPx(size)} className={styles.glyph} data-flx="connection.connection-icon.glyph" />
			);
		case ConnectionTypes.OAUTH: {
			const provider = findConnectionProvider(connection);
			return provider ? (
				<ProviderIcon provider={provider} size={size} data-flx="connection.connection-icon.provider-icon" />
			) : (
				<LinkBreakIcon size={remFromPx(size)} className={styles.glyph} data-flx="connection.connection-icon.glyph--2" />
			);
		}
	}
});
