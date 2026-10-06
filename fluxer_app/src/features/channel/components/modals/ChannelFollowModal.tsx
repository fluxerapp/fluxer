// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {
	createGuildComboboxRenderers,
	type GuildComboboxOption,
} from '@app/features/app/components/dialogs/shared/GuildComboboxRenderers';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import * as ChannelFollowCommands from '@app/features/channel/commands/ChannelFollowCommands';
import styles from '@app/features/channel/components/modals/ChannelFollowModal.module.css';
import {openChannelFollowSuccessModal} from '@app/features/channel/components/modals/ChannelFollowSuccessModal';
import Channels from '@app/features/channel/state/Channels';
import {
	type ChannelFollowTarget,
	FOLLOW_DESCRIPTOR,
	getChannelFollowSourceRestriction,
	getFollowTargetChannels,
	getFollowTargetGuilds,
} from '@app/features/channel/utils/ChannelFollowUtils';
import {UNKNOWN_CHANNEL_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import Guilds from '@app/features/guild/state/Guilds';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Combobox, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import foodPatternUrl from '@app/media/images/i-like-food.svg';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo} from 'react';
import {Controller, useForm} from 'react-hook-form';

const FOLLOW_CHANNEL_TITLE_DESCRIPTOR = msg({
	message: 'Follow this channel',
	comment:
		'Title of the dialog that follows an announcement channel, so its published messages are copied into a channel of a community the user picks.',
});
const SEND_TO_LABEL_DESCRIPTOR = msg({
	message: 'Community',
	comment: 'Field label in the follow announcement channel dialog. The user picks the community that receives updates.',
});
const SELECT_CHANNEL_LABEL_DESCRIPTOR = msg({
	message: 'Channel',
	comment:
		'Field label in the follow announcement channel dialog. The user picks the text channel that receives updates.',
});
const SELECT_A_COMMUNITY_DESCRIPTOR = msg({
	message: 'Select a community',
	comment: 'Placeholder of the community picker in the follow announcement channel dialog.',
});
const SELECT_A_CHANNEL_DESCRIPTOR = msg({
	message: 'Select a channel',
	comment: 'Placeholder of the channel picker in the follow announcement channel dialog.',
});

interface FormInputs {
	guildId: string;
	channelId: string;
}

interface ChannelFollowTargetOption extends ComboboxOption {
	target: ChannelFollowTarget;
}

export const ChannelFollowModal = observer(({channelId}: {channelId: string}) => {
	const {i18n} = useLingui();
	const source = Channels.getChannel(channelId);
	const sourceGuild = source?.guildId ? Guilds.getGuild(source.guildId) : undefined;
	const restriction = source ? getChannelFollowSourceRestriction(source) : null;
	const guilds = getFollowTargetGuilds(restriction);
	const form = useForm<FormInputs>({defaultValues: {guildId: '', channelId: ''}});
	const selectedGuildId = form.watch('guildId');
	const selectedChannelId = form.watch('channelId');
	const targets = selectedGuildId ? getFollowTargetChannels(selectedGuildId, restriction) : [];
	const guildOptions: Array<GuildComboboxOption> = guilds.map((guild) => ({
		value: guild.id,
		label: guild.name,
		iconUrl: guild.icon ? AvatarUtils.getGuildIconURL({id: guild.id, icon: guild.icon}) : null,
	}));
	const channelOptions: Array<ChannelFollowTargetOption> = targets.map((target) => ({
		value: target.channel.id,
		label: target.channel.name ?? i18n._(UNKNOWN_CHANNEL_DESCRIPTOR),
		target,
	}));
	const guildRenderers = useMemo(
		() =>
			createGuildComboboxRenderers<GuildComboboxOption>({
				styles: {
					optionRow: styles.guildOption,
					avatar: styles.guildAvatar,
					avatarPlaceholder: styles.guildAvatarPlaceholder,
					label: styles.optionLabel,
				},
			}),
		[],
	);
	const renderChannelOption = useCallback(
		(option: ChannelFollowTargetOption) => (
			<div className={styles.channelOption} data-flx="channel.channel-follow-modal.channel-option">
				{ChannelUtils.getIcon(option.target.channel, {className: styles.channelIcon})}
				<span className={styles.optionLabel} data-flx="channel.channel-follow-modal.channel-option.label">
					{option.label}
				</span>
				{option.target.categoryName && (
					<span className={styles.categoryName} data-flx="channel.channel-follow-modal.channel-option.category">
						{option.target.categoryName}
					</span>
				)}
			</div>
		),
		[],
	);
	const onSubmit = async (data: FormInputs) => {
		if (!data.channelId) return;
		await ChannelFollowCommands.followChannel(channelId, data.channelId);
		ModalCommands.pop();
		openChannelFollowSuccessModal({sourceChannelId: channelId, targetChannelId: data.channelId});
	};
	const {handleSubmit} = useFormSubmit({form, onSubmit, defaultErrorField: 'channelId'});
	const hasGuilds = guilds.length > 0;
	return (
		<Modal.Root size="small" centered data-flx="channel.channel-follow-modal.modal-root">
			<Modal.ScreenReaderLabel
				text={i18n._(FOLLOW_CHANNEL_TITLE_DESCRIPTOR)}
				data-flx="channel.channel-follow-modal.modal-screen-reader-label"
			/>
			<Form form={form} onSubmit={handleSubmit} data-flx="channel.channel-follow-modal.form.submit">
				<div className={styles.banner} aria-hidden data-flx="channel.channel-follow-modal.banner">
					<div
						className={styles.patternImage}
						style={{backgroundImage: `url(${foodPatternUrl})`}}
						data-flx="channel.channel-follow-modal.pattern-image"
					/>
					<div className={styles.bannerRow} data-flx="channel.channel-follow-modal.banner-row">
						{sourceGuild && (
							<GuildIcon
								id={sourceGuild.id}
								name={sourceGuild.name}
								icon={sourceGuild.icon}
								sizePx={48}
								className={styles.sourceGuildIcon}
								containerProps={{'data-flx': 'channel.channel-follow-modal.source-guild-icon'}}
								data-flx="channel.channel-follow-modal.guild-icon"
							/>
						)}
						{source && (
							<div className={styles.sourceChip} data-flx="channel.channel-follow-modal.source-chip">
								{ChannelUtils.getIcon(source, {className: styles.sourceChipIcon})}
								<span className={styles.sourceChipName} data-flx="channel.channel-follow-modal.source-chip-name">
									{source.name ?? i18n._(UNKNOWN_CHANNEL_DESCRIPTOR)}
								</span>
							</div>
						)}
					</div>
				</div>
				<Modal.Content contentClassName={styles.content} data-flx="channel.channel-follow-modal.modal-content">
					<div className={styles.intro} data-flx="channel.channel-follow-modal.intro">
						<h2 className={styles.title} data-flx="channel.channel-follow-modal.title">
							{i18n._(FOLLOW_CHANNEL_TITLE_DESCRIPTOR)}
						</h2>
						<p className={styles.description} data-flx="channel.channel-follow-modal.description">
							<Trans comment="Subtitle of the follow announcement channel dialog. Community settings and Webhooks are the names of the settings page and its tab where followed channels are listed.">
								Choose where its published messages should go. You can unfollow any time in Community settings →
								Webhooks.
							</Trans>
						</p>
					</div>
					{restriction === 'age_restricted' && (
						<WarningAlert data-flx="channel.channel-follow-modal.age-restricted-warning">
							<Trans comment="Warning in the follow announcement channel dialog when the followed channel is age-restricted.">
								This is an age-restricted channel. Updates can only go to age-restricted channels.
							</Trans>
						</WarningAlert>
					)}
					{restriction === 'content_warning' && (
						<WarningAlert data-flx="channel.channel-follow-modal.content-warning-warning">
							<Trans comment="Warning in the follow announcement channel dialog when the followed channel has a content warning.">
								This channel has a content warning. Updates can only go to channels with a content warning or an age
								restriction.
							</Trans>
						</WarningAlert>
					)}
					{hasGuilds ? (
						<>
							<Controller
								name="guildId"
								control={form.control}
								render={({field}) => (
									<Combobox<string, false, GuildComboboxOption>
										label={i18n._(SEND_TO_LABEL_DESCRIPTOR)}
										value={field.value}
										options={guildOptions}
										onChange={(value) => {
											field.onChange(value);
											form.setValue('channelId', '');
											form.clearErrors('channelId');
										}}
										placeholder={i18n._(SELECT_A_COMMUNITY_DESCRIPTOR)}
										renderOption={guildRenderers.renderOption}
										renderValue={guildRenderers.renderValue}
										data-flx="channel.channel-follow-modal.guild-select"
									/>
								)}
								data-flx="channel.channel-follow-modal.guild-controller"
							/>
							<Controller
								name="channelId"
								control={form.control}
								render={({field, fieldState}) => (
									<Combobox<string, false, ChannelFollowTargetOption>
										label={i18n._(SELECT_CHANNEL_LABEL_DESCRIPTOR)}
										value={field.value}
										options={channelOptions}
										onChange={(value) => field.onChange(value)}
										placeholder={i18n._(SELECT_A_CHANNEL_DESCRIPTOR)}
										disabled={!selectedGuildId}
										error={fieldState.error?.message}
										renderOption={renderChannelOption}
										renderValue={(option) => (option ? renderChannelOption(option) : null)}
										data-flx="channel.channel-follow-modal.channel-select"
									/>
								)}
								data-flx="channel.channel-follow-modal.channel-controller"
							/>
							<p className={styles.hint} data-flx="channel.channel-follow-modal.hint">
								<Trans comment="Footnote under the pickers in the follow announcement channel dialog.">
									Communities and channels where you can't manage webhooks are hidden.
								</Trans>
							</p>
						</>
					) : (
						<div className={styles.emptyState} data-flx="channel.channel-follow-modal.empty-state">
							<Trans comment="Empty state of the follow announcement channel dialog when the user cannot manage webhooks anywhere.">
								You can't manage webhooks in any community. Ask an admin to follow this channel.
							</Trans>
						</div>
					)}
				</Modal.Content>
				<Modal.Footer data-flx="channel.channel-follow-modal.modal-footer">
					<Button onClick={ModalCommands.pop} variant="secondary" data-flx="channel.channel-follow-modal.button.cancel">
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
					<Button
						type="submit"
						disabled={!hasGuilds || !selectedGuildId || !selectedChannelId}
						submitting={form.formState.isSubmitting}
						data-flx="channel.channel-follow-modal.button.submit"
					>
						{i18n._(FOLLOW_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});

export function openChannelFollowModal(channelId: string): void {
	ModalCommands.push(
		modal(() => <ChannelFollowModal channelId={channelId} data-flx="channel.channel-follow-modal.open" />),
	);
}
