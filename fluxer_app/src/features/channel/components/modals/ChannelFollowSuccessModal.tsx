// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import styles from '@app/features/channel/components/modals/ChannelFollowSuccessModal.module.css';
import Channels from '@app/features/channel/state/Channels';
import {UNKNOWN_CHANNEL_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import Guilds from '@app/features/guild/state/Guilds';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {AnnouncementChannelIcon} from '@app/features/ui/components/icons/AnnouncementChannelIcon';
import foodPatternUrl from '@app/media/images/i-like-food.svg';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowRightIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const FOLLOW_SUCCESS_TITLE_DESCRIPTOR = msg({
	message: 'Updates are on their way!',
	comment: 'Heading of the dialog shown after the user follows an announcement channel.',
});
const FOLLOW_SUCCESS_BODY_DESCRIPTOR = msg({
	message: 'Messages published in #{sourceName} will now appear in #{targetName}.',
	comment:
		'Body of the dialog shown after the user follows an announcement channel. sourceName is the followed announcement channel and targetName the channel that receives its updates, both without the leading #.',
});
const GOT_IT_DESCRIPTOR = msg({
	message: 'Got it!',
	comment: 'Button that closes the dialog shown after the user follows an announcement channel.',
});

interface ChannelFollowSuccessModalProps {
	sourceChannelId: string;
	targetChannelId: string;
}

export const ChannelFollowSuccessModal = observer(
	({sourceChannelId, targetChannelId}: ChannelFollowSuccessModalProps) => {
		const {i18n} = useLingui();
		const source = Channels.getChannel(sourceChannelId);
		const target = Channels.getChannel(targetChannelId);
		const sourceGuild = source?.guildId ? Guilds.getGuild(source.guildId) : undefined;
		const targetGuild = target?.guildId ? Guilds.getGuild(target.guildId) : undefined;
		const title = i18n._(FOLLOW_SUCCESS_TITLE_DESCRIPTOR);
		return (
			<Modal.Root size="small" centered data-flx="channel.channel-follow-success-modal.modal-root">
				<Modal.ScreenReaderLabel
					text={title}
					data-flx="channel.channel-follow-success-modal.modal-screen-reader-label"
				/>
				<div className={styles.hero} aria-hidden data-flx="channel.channel-follow-success-modal.hero">
					<div
						className={styles.patternImage}
						style={{backgroundImage: `url(${foodPatternUrl})`}}
						data-flx="channel.channel-follow-success-modal.pattern-image"
					/>
					<div className={styles.composition} data-flx="channel.channel-follow-success-modal.composition">
						{sourceGuild && (
							<div className={styles.sourceIconWrap} data-flx="channel.channel-follow-success-modal.source-icon-wrap">
								<GuildIcon
									id={sourceGuild.id}
									name={sourceGuild.name}
									icon={sourceGuild.icon}
									sizePx={56}
									className={styles.guildIcon}
									containerProps={{'data-flx': 'channel.channel-follow-success-modal.source-guild-icon'}}
									data-flx="channel.channel-follow-success-modal.source-guild-icon"
								/>
								<span
									className={styles.announcementBadge}
									data-flx="channel.channel-follow-success-modal.announcement-badge"
								>
									<AnnouncementChannelIcon
										className={styles.announcementBadgeIcon}
										data-flx="channel.channel-follow-success-modal.announcement-badge-icon"
									/>
								</span>
							</div>
						)}
						<ArrowRightIcon
							weight="bold"
							className={styles.arrow}
							data-flx="channel.channel-follow-success-modal.arrow-right-icon"
						/>
						{targetGuild && (
							<GuildIcon
								id={targetGuild.id}
								name={targetGuild.name}
								icon={targetGuild.icon}
								sizePx={56}
								className={styles.guildIcon}
								containerProps={{'data-flx': 'channel.channel-follow-success-modal.target-guild-icon'}}
								data-flx="channel.channel-follow-success-modal.target-guild-icon"
							/>
						)}
					</div>
				</div>
				<div className={styles.body} data-flx="channel.channel-follow-success-modal.body">
					<h2 className={styles.title} data-flx="channel.channel-follow-success-modal.title">
						{title}
					</h2>
					<p className={styles.description} data-flx="channel.channel-follow-success-modal.description">
						{i18n._(FOLLOW_SUCCESS_BODY_DESCRIPTOR, {
							sourceName: source?.name ?? i18n._(UNKNOWN_CHANNEL_DESCRIPTOR),
							targetName: target?.name ?? i18n._(UNKNOWN_CHANNEL_DESCRIPTOR),
						})}
					</p>
					<Button onClick={ModalCommands.pop} data-flx="channel.channel-follow-success-modal.button.got-it">
						{i18n._(GOT_IT_DESCRIPTOR)}
					</Button>
				</div>
			</Modal.Root>
		);
	},
);

export function openChannelFollowSuccessModal(props: ChannelFollowSuccessModalProps): void {
	ModalCommands.push(
		modal(() => <ChannelFollowSuccessModal {...props} data-flx="channel.channel-follow-success-modal.open" />),
	);
}
