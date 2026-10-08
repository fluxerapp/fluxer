// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import styles from '@app/features/channel/components/embeds/ChannelEmbed.module.css';
import {
	type LinkComponentProps,
	logger,
} from '@app/features/channel/components/embeds/channel_embed/ChannelEmbedShared';
import {SUPPRESS_EMBEDS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {useOpenInBrowserOnMiddleClick} from '@app/features/messaging/hooks/useOpenInBrowserOnMiddleClick';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import TrustedDomain from '@app/features/trusted_domain/state/TrustedDomain';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import type {FC} from 'react';

export const EmbedLink: FC<LinkComponentProps> = observer(({url, children, className}) => {
	const handleClick = (e: React.MouseEvent<HTMLAnchorElement>) => {
		e.stopPropagation();
		try {
			const parsed = new URL(url);
			if (!TrustedDomain.isTrustedDomain(parsed.hostname)) {
				e.preventDefault();
				openExternalUrlWithWarning(url);
			}
		} catch (_error) {
			logger.warn('Invalid URL in embed link:', url);
		}
	};
	const openInBrowser = useOpenInBrowserOnMiddleClick(url);
	return (
		<FocusRing data-flx="channel.embeds.embed.link-component.focus-ring">
			<a
				className={clsx(styles.embedLink, className)}
				href={url}
				rel="noopener noreferrer"
				target="_blank"
				onClick={handleClick}
				onAuxClick={openInBrowser.onAuxClick}
				data-flx="channel.embeds.embed.link-component.a"
			>
				{children}
			</a>
		</FocusRing>
	);
});
export const SuppressEmbedsConfirmModal: FC<{message: Message}> = ({message}) => {
	const {i18n} = useLingui();
	return (
		<ConfirmModal
			title={i18n._(SUPPRESS_EMBEDS_DESCRIPTOR)}
			description={
				<Trans>
					Are you sure you want to suppress all link embeds on this message? This action will hide all embeds from this
					message.
				</Trans>
			}
			primaryText={i18n._(SUPPRESS_EMBEDS_DESCRIPTOR)}
			primaryVariant="danger"
			onPrimary={async () => {
				await MessageCommands.toggleSuppressEmbeds(message.channelId, message.id, message.flags);
			}}
			data-flx="channel.embeds.embed.suppress-embeds-confirm-modal.confirm-modal"
		/>
	);
};
