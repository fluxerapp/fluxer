// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {EXAMPLE_CHANNEL_NAME, EXAMPLE_URL} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import {ChannelCreateAccessStep} from '@app/features/channel/components/modals/ChannelCreateAccessStep';
import styles from '@app/features/channel/components/modals/ChannelCreateModal.module.css';
import {
	buildPrivateChannelOverwrites,
	type ChannelCreateOverwrite,
	createChannel,
	type FormInputs,
	getChannelTypeOptions,
	getDefaultValues,
} from '@app/features/channel/utils/ChannelCreateModalUtils';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {
	CANCEL_DESCRIPTOR,
	CREATE_CHANNEL_DESCRIPTOR,
	GO_BACK_DESCRIPTOR,
	NEXT_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import Users from '@app/features/user/state/Users';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {LockSimpleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';
import {Controller, useForm} from 'react-hook-form';

const CHANNEL_TYPE_DESCRIPTOR = msg({
	message: 'Channel type',
	comment: 'Short label in the channel create modal. Keep it concise.',
});
const CHANNEL_TYPE_SELECTION_DESCRIPTOR = msg({
	message: 'Channel type selection',
	comment: 'Short label in the channel create modal. Keep it concise.',
});
const CHANNEL_NAME_DESCRIPTOR = msg({
	message: 'Channel name',
	comment: 'Label of the name field in the channel create modal.',
});
const URL_DESCRIPTOR = msg({
	message: 'URL',
	comment: 'Short label in the channel create modal. Keep it concise.',
});
const PRIVATE_CHANNEL_DESCRIPTOR = msg({
	message: 'Private channel',
	comment: 'Switch in the channel create modal that limits who can view the new channel.',
});
const PRIVATE_CHANNEL_HINT_DESCRIPTOR = msg({
	message: 'Only selected members and roles will be able to view this channel.',
	comment: 'Description under the private channel switch in the channel create modal.',
});
const ADD_MEMBERS_OR_ROLES_DESCRIPTOR = msg({
	message: 'Add members or roles',
	comment: 'Title of the second step of the channel create modal when the channel is private.',
});
const SKIP_DESCRIPTOR = msg({
	message: 'Skip',
	comment: 'Creates the private channel without adding anyone in the channel create modal.',
});

type Step = 'details' | 'access';

export const ChannelCreateModal = observer(({guildId, parentId}: {guildId: string; parentId?: string}) => {
	const {i18n} = useLingui();
	const [step, setStep] = useState<Step>('details');
	const [isPrivate, setIsPrivate] = useState(false);
	const [access, setAccess] = useState<Array<ChannelCreateOverwrite>>([]);
	const form = useForm<FormInputs>({
		defaultValues: getDefaultValues(),
	});
	const channelType = Number(form.watch('type') || '0');
	const canBePrivate = channelType !== ChannelTypes.GUILD_LINK;
	const privateSelected = isPrivate && canBePrivate;
	const onSubmit = async (data: FormInputs) => {
		if (privateSelected && step === 'details') {
			setStep('access');
			return;
		}
		const currentUserId = Users.currentUser?.id;
		const members =
			currentUserId && !access.some((entry) => entry.type === 1 && entry.id === currentUserId)
				? [...access, {id: currentUserId, type: 1 as const}]
				: access;
		await createChannel(
			guildId,
			data,
			parentId,
			privateSelected ? buildPrivateChannelOverwrites(guildId, Number(data.type), members) : undefined,
		);
	};
	const {handleSubmit} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});
	const typeIcon = ChannelUtils.getIcon(
		{
			type: channelType,
			guildId,
			permissionOverwrites: privateSelected ? {[guildId]: {deny: Permissions.VIEW_CHANNEL}} : undefined,
		},
		{className: styles.nameIcon},
	);
	const submitting = form.formState.isSubmitting;
	return (
		<Modal.Root size="small" centered data-flx="channel.channel-create-modal.modal-root">
			<Form form={form} onSubmit={handleSubmit} data-flx="channel.channel-create-modal.form.submit">
				<Modal.Header
					title={i18n._(step === 'access' ? ADD_MEMBERS_OR_ROLES_DESCRIPTOR : CREATE_CHANNEL_DESCRIPTOR)}
					data-flx="channel.channel-create-modal.modal-header"
				/>
				<Modal.Content contentClassName={styles.content} data-flx="channel.channel-create-modal.modal-content">
					{step === 'access' ? (
						<ChannelCreateAccessStep
							guildId={guildId}
							selected={access}
							onChange={setAccess}
							data-flx="channel.channel-create-modal.channel-create-access-step"
						/>
					) : (
						<>
							<section className={styles.section} data-flx="channel.channel-create-modal.channel-type-section">
								<h3 className={styles.sectionLabel} data-flx="channel.channel-create-modal.channel-type-label">
									{i18n._(CHANNEL_TYPE_DESCRIPTOR)}
								</h3>
								<Controller
									name="type"
									control={form.control}
									render={({field}) => (
										<RadioGroup
											aria-label={i18n._(CHANNEL_TYPE_SELECTION_DESCRIPTOR)}
											value={Number(field.value)}
											onChange={(value) => field.onChange(value.toString())}
											options={getChannelTypeOptions(i18n, {forums: ThreadGuilds.isActive(guildId)})}
											className={styles.typeGroup}
											optionAlign="center"
											renderContent={(option, checked) => (
												<span className={styles.typeOption} data-flx="channel.channel-create-modal.type-option">
													<span
														className={checked ? styles.typeIconSelected : styles.typeIcon}
														data-flx="channel.channel-create-modal.type-icon"
													>
														{ChannelUtils.getIcon({type: option.value}, {className: styles.typeGlyph})}
													</span>
													<span className={styles.typeText} data-flx="channel.channel-create-modal.type-text">
														<span className={styles.typeName} data-flx="channel.channel-create-modal.type-name">
															{option.name}
														</span>
														<span className={styles.typeDesc} data-flx="channel.channel-create-modal.type-desc">
															{option.desc}
														</span>
													</span>
												</span>
											)}
											data-flx="channel.channel-create-modal.radio-group.change"
										/>
									)}
									data-flx="channel.channel-create-modal.controller"
								/>
							</section>
							<Input
								data-flx="channel.channel-create-modal.input"
								{...form.register('name')}
								autoComplete="off"
								autoFocus={true}
								error={form.formState.errors.name?.message}
								label={i18n._(CHANNEL_NAME_DESCRIPTOR)}
								leftIcon={typeIcon}
								maxLength={100}
								minLength={1}
								placeholder={EXAMPLE_CHANNEL_NAME}
								required={true}
							/>
							{channelType === ChannelTypes.GUILD_LINK && (
								<Input
									data-flx="channel.channel-create-modal.input.url"
									{...form.register('url')}
									error={form.formState.errors.url?.message}
									label={i18n._(URL_DESCRIPTOR)}
									maxLength={1024}
									placeholder={EXAMPLE_URL}
									required={true}
									type="url"
								/>
							)}
							{canBePrivate && (
								<Switch
									label={
										<span className={styles.privateLabel} data-flx="channel.channel-create-modal.private-label">
											<LockSimpleIcon
												size={16}
												weight="fill"
												className={styles.privateIcon}
												aria-hidden
												data-flx="channel.channel-create-modal.lock-icon"
											/>
											{i18n._(PRIVATE_CHANNEL_DESCRIPTOR)}
										</span>
									}
									description={i18n._(PRIVATE_CHANNEL_HINT_DESCRIPTOR)}
									value={isPrivate}
									onChange={setIsPrivate}
									data-flx="channel.channel-create-modal.switch.private"
								/>
							)}
						</>
					)}
				</Modal.Content>
				<Modal.Footer data-flx="channel.channel-create-modal.modal-footer">
					{step === 'access' ? (
						<>
							<Button
								onClick={() => setStep('details')}
								variant="secondary"
								data-flx="channel.channel-create-modal.button.back"
							>
								{i18n._(GO_BACK_DESCRIPTOR)}
							</Button>
							<Button type="submit" submitting={submitting} data-flx="channel.channel-create-modal.button.submit">
								{i18n._(access.length === 0 ? SKIP_DESCRIPTOR : CREATE_CHANNEL_DESCRIPTOR)}
							</Button>
						</>
					) : (
						<>
							<Button
								onClick={ModalCommands.pop}
								variant="secondary"
								data-flx="channel.channel-create-modal.button.pop"
							>
								{i18n._(CANCEL_DESCRIPTOR)}
							</Button>
							<Button type="submit" submitting={submitting} data-flx="channel.channel-create-modal.button.submit">
								{i18n._(privateSelected ? NEXT_DESCRIPTOR : CREATE_CHANNEL_DESCRIPTOR)}
							</Button>
						</>
					)}
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
