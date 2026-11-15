// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import type {Channel} from '@app/features/channel/models/Channel';
import * as GuildMatureContentCommands from '@app/features/guild/commands/GuildMatureContentCommands';
import GuildMatureContentAgree, {
	MatureContentGateReason,
	type ResolvedGateContext,
} from '@app/features/guild/state/GuildMatureContentAgree';
import {
	CANCEL_DESCRIPTOR,
	CONTINUE_DESCRIPTOR,
	MATURE_CONTENT_DESCRIPTOR,
	OPEN_LINK_DESCRIPTOR,
	PROCEED_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getDefaultContentWarningText} from '@app/features/messaging/utils/ContentWarningUtils';
import {getEffectiveMatureContentGeoContext} from '@app/features/moderation/utils/MatureContentGeoUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {getRegionDisplayName} from '@app/features/user/utils/UserGeo';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo, useRef} from 'react';

const COMMUNITY_CONTENT_WARNING_DESCRIPTOR = msg({
	message: 'Community content warning',
	comment: 'Title in a modal shown before opening a community with a custom content warning.',
});
const CATEGORY_CONTENT_WARNING_DESCRIPTOR = msg({
	message: 'Category content warning',
	comment: 'Title in a modal shown before opening a category with a custom content warning.',
});
const CHANNEL_CONTENT_WARNING_DESCRIPTOR = msg({
	message: 'Channel content warning',
	comment: 'Title in a modal shown before opening a channel with a custom content warning.',
});
const MATURE_CONTENT_REGION_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Due to mature content laws in {regionName}, this content is not available from here.',
	comment:
		'Body in the channel gate modal for regions where local law blocks mature content. {regionName} is a localized place name.',
});
const MATURE_COMMUNITY_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'This mature community is not available to your account.',
	comment: 'Body in the channel gate modal when the current user cannot access a mature community.',
});
const MATURE_CATEGORY_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'This mature category is not available to your account.',
	comment: 'Body in the channel gate modal when the current user cannot access a mature category.',
});
const MATURE_CHANNEL_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'This mature channel is not available to your account.',
	comment: 'Body in the channel gate modal when the current user cannot access a mature channel.',
});
const MATURE_COMMUNITY_BODY_DESCRIPTOR = msg({
	message:
		'This community is marked for mature content and may contain material that may be inappropriate for some users.',
	comment: 'Body in the channel gate modal for a mature community when no custom warning is set.',
});
const MATURE_CATEGORY_BODY_DESCRIPTOR = msg({
	message:
		'This category is marked for mature content and may contain material that may be inappropriate for some users.',
	comment: 'Body in the channel gate modal for a mature category when no custom warning is set.',
});
const MATURE_VOICE_CHANNEL_BODY_DESCRIPTOR = msg({
	message:
		'This voice channel is marked for mature content and may contain material that may be inappropriate for some users.',
	comment: 'Body in the channel gate modal for a mature voice channel when no custom warning is set.',
});
const MATURE_LINK_CHANNEL_BODY_DESCRIPTOR = msg({
	message:
		'This link channel is marked for mature content and may open material that may be inappropriate for some users.',
	comment: 'Body in the channel gate modal for a mature link channel when no custom warning is set.',
});
const MATURE_CHANNEL_BODY_DESCRIPTOR = msg({
	message:
		'This channel is marked for mature content and may contain material that may be inappropriate for some users.',
	comment: 'Body in the channel gate modal for a mature text channel when no custom warning is set.',
});

interface ChannelAccessGateModalProps {
	channel?: Channel | null;
	channelId?: string | null;
	guildId?: string | null;
	reason: MatureContentGateReason;
	onConfirm?: () => void;
}

function agreeToResolvedScope(resolved: ResolvedGateContext, fallbackChannelId?: string | null): void {
	if (resolved.scope === 'guild' && resolved.guildId) {
		GuildMatureContentCommands.agreeToGuild(resolved.guildId);
		return;
	}
	if (resolved.scope === 'category' && (resolved.scopeId || resolved.categoryId)) {
		GuildMatureContentCommands.agreeToCategory((resolved.scopeId ?? resolved.categoryId) as string);
		return;
	}
	if (resolved.scopeId) {
		GuildMatureContentCommands.agreeToChannel(resolved.scopeId);
		return;
	}
	if (fallbackChannelId) {
		GuildMatureContentCommands.agreeToChannel(fallbackChannelId);
	}
}

export const ChannelAccessGateModal = observer(
	({channel, channelId, guildId, reason, onConfirm}: ChannelAccessGateModalProps) => {
		const {i18n} = useLingui();
		const initialFocusRef = useRef<HTMLButtonElement | null>(null);
		const resolved = GuildMatureContentAgree.getResolvedContext({channelId: channelId ?? channel?.id ?? null, guildId});
		const hasCustomWarningText = resolved.effectiveWarningText != null && resolved.effectiveWarningText.length > 0;
		const warningBody = hasCustomWarningText ? resolved.effectiveWarningText : getDefaultContentWarningText(i18n);
		const isVoiceChannel = channel?.type === ChannelTypes.GUILD_VOICE;
		const isLinkChannel = channel?.type === ChannelTypes.GUILD_LINK;
		const title = useMemo(() => {
			if (
				reason === MatureContentGateReason.GEO_RESTRICTED ||
				reason === MatureContentGateReason.MATURE_CONTENT_CHECK_REQUIRED
			) {
				return i18n._(MATURE_CONTENT_DESCRIPTOR);
			}
			if (resolved.effectiveMatureContent) {
				return i18n._(MATURE_CONTENT_DESCRIPTOR);
			}
			if (resolved.scope === 'guild') {
				return i18n._(COMMUNITY_CONTENT_WARNING_DESCRIPTOR);
			}
			if (resolved.scope === 'category') {
				return i18n._(CATEGORY_CONTENT_WARNING_DESCRIPTOR);
			}
			return i18n._(CHANNEL_CONTENT_WARNING_DESCRIPTOR);
		}, [i18n.locale, reason, resolved.effectiveMatureContent, resolved.scope]);
		const body = useMemo(() => {
			if (reason === MatureContentGateReason.GEO_RESTRICTED) {
				const {countryCode, regionCode} = getEffectiveMatureContentGeoContext();
				const regionName = getRegionDisplayName(i18n, countryCode ?? undefined, regionCode ?? undefined);
				return i18n._(MATURE_CONTENT_REGION_UNAVAILABLE_DESCRIPTOR, {regionName});
			}
			if (reason === MatureContentGateReason.MATURE_CONTENT_CHECK_REQUIRED) {
				if (resolved.scope === 'guild') {
					return i18n._(MATURE_COMMUNITY_UNAVAILABLE_DESCRIPTOR);
				}
				if (resolved.scope === 'category') {
					return i18n._(MATURE_CATEGORY_UNAVAILABLE_DESCRIPTOR);
				}
				return i18n._(MATURE_CHANNEL_UNAVAILABLE_DESCRIPTOR);
			}
			if (!resolved.effectiveMatureContent) {
				return warningBody;
			}
			if (hasCustomWarningText) {
				return warningBody;
			}
			if (resolved.scope === 'guild') {
				return i18n._(MATURE_COMMUNITY_BODY_DESCRIPTOR);
			}
			if (resolved.scope === 'category') {
				return i18n._(MATURE_CATEGORY_BODY_DESCRIPTOR);
			}
			if (isVoiceChannel) {
				return i18n._(MATURE_VOICE_CHANNEL_BODY_DESCRIPTOR);
			}
			if (isLinkChannel) {
				return i18n._(MATURE_LINK_CHANNEL_BODY_DESCRIPTOR);
			}
			return i18n._(MATURE_CHANNEL_BODY_DESCRIPTOR);
		}, [
			i18n.locale,
			reason,
			resolved.scope,
			resolved.effectiveMatureContent,
			warningBody,
			hasCustomWarningText,
			isVoiceChannel,
			isLinkChannel,
		]);
		const handleCancel = useCallback(() => {
			ModalCommands.pop();
		}, []);
		const handleConfirm = useCallback(() => {
			agreeToResolvedScope(resolved, channelId ?? channel?.id ?? null);
			ModalCommands.pop();
			onConfirm?.();
		}, [channel?.id, channelId, onConfirm, resolved]);
		const canConfirm = reason === MatureContentGateReason.CONSENT_REQUIRED;
		let confirmDescriptor = CONTINUE_DESCRIPTOR;
		if (canConfirm) {
			if (resolved.effectiveMatureContent) {
				confirmDescriptor = PROCEED_DESCRIPTOR;
			} else if (isLinkChannel) {
				confirmDescriptor = OPEN_LINK_DESCRIPTOR;
			}
		}
		return (
			<Modal.Root
				size="small"
				centered
				onClose={handleCancel}
				initialFocusRef={initialFocusRef}
				data-flx="channel.channel-access-gate-modal.modal-root"
			>
				<Modal.Header title={title} data-flx="channel.channel-access-gate-modal.modal-header" />
				<Modal.Content data-flx="channel.channel-access-gate-modal.modal-content">
					<Modal.ContentLayout data-flx="channel.channel-access-gate-modal.modal-content-layout">
						<Modal.Description data-flx="channel.channel-access-gate-modal.modal-description">{body}</Modal.Description>
					</Modal.ContentLayout>
				</Modal.Content>
				{canConfirm ? (
					<Modal.Footer data-flx="channel.channel-access-gate-modal.modal-footer">
						<Button
							type="button"
							variant="secondary"
							onClick={handleCancel}
							data-flx="channel.channel-access-gate-modal.button.cancel"
						>
							{i18n._(CANCEL_DESCRIPTOR)}
						</Button>
						<Button
							type="button"
							ref={initialFocusRef}
							variant={resolved.effectiveMatureContent ? 'danger' : 'primary'}
							onClick={handleConfirm}
							data-flx="channel.channel-access-gate-modal.button.primary"
						>
							{i18n._(confirmDescriptor)}
						</Button>
					</Modal.Footer>
				) : (
					<Modal.DismissFooter data-flx="channel.channel-access-gate-modal.dismiss-footer" />
				)}
			</Modal.Root>
		);
	},
);
