// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {EXAMPLE_CHANNEL_NAME, EXAMPLE_URL} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import styles from '@app/features/channel/components/modals/ChannelCreateModal.module.css';
import {
	createChannel,
	type FormInputs,
	getChannelTypeOptions,
	getDefaultValues,
} from '@app/features/channel/utils/ChannelCreateModalUtils';
import {CANCEL_DESCRIPTOR, CREATE_CHANNEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {Controller, useForm} from 'react-hook-form';

const CHANNEL_TYPE_DESCRIPTOR = msg({
	message: 'Channel type',
	comment: 'Short label in the channel create modal. Keep it concise.',
});
const CHANNEL_TYPE_SELECTION_DESCRIPTOR = msg({
	message: 'Channel type selection',
	comment: 'Short label in the channel create modal. Keep it concise.',
});
const NAME_DESCRIPTOR = msg({
	message: 'Name',
	comment: 'Short label in the channel create modal. Keep it concise.',
});
const URL_DESCRIPTOR = msg({
	message: 'URL',
	comment: 'Short label in the channel create modal. Keep it concise.',
});
export const ChannelCreateModal = observer(({guildId, parentId}: {guildId: string; parentId?: string}) => {
	const {i18n} = useLingui();
	const form = useForm<FormInputs>({
		defaultValues: getDefaultValues(),
	});
	const onSubmit = async (data: FormInputs) => {
		await createChannel(guildId, data, parentId);
	};
	const {handleSubmit} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});
	return (
		<Modal.Root size="small" centered data-flx="channel.channel-create-modal.modal-root">
			<Form form={form} onSubmit={handleSubmit} data-flx="channel.channel-create-modal.form.submit">
				<Modal.Header title={i18n._(CREATE_CHANNEL_DESCRIPTOR)} data-flx="channel.channel-create-modal.modal-header" />
				<Modal.Content contentClassName={styles.content} data-flx="channel.channel-create-modal.modal-content">
					<div className={styles.channelTypeSection} data-flx="channel.channel-create-modal.channel-type-section">
						<div className={styles.channelTypeLabel} data-flx="channel.channel-create-modal.channel-type-label">
							{i18n._(CHANNEL_TYPE_DESCRIPTOR)}
						</div>
						<Controller
							name="type"
							control={form.control}
							render={({field}) => (
								<RadioGroup
									aria-label={i18n._(CHANNEL_TYPE_SELECTION_DESCRIPTOR)}
									value={Number(field.value)}
									onChange={(value) => field.onChange(value.toString())}
									options={getChannelTypeOptions(i18n, {forums: ThreadGuilds.isActive(guildId)})}
									data-flx="channel.channel-create-modal.radio-group.change"
								/>
							)}
							data-flx="channel.channel-create-modal.controller"
						/>
					</div>
					<Input
						data-flx="channel.channel-create-modal.input"
						{...form.register('name')}
						autoComplete="off"
						autoFocus={true}
						error={form.formState.errors.name?.message}
						label={i18n._(NAME_DESCRIPTOR)}
						maxLength={100}
						minLength={1}
						placeholder={EXAMPLE_CHANNEL_NAME}
						required={true}
					/>
					{Number(form.watch('type') || '0') === ChannelTypes.GUILD_LINK && (
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
				</Modal.Content>
				<Modal.Footer data-flx="channel.channel-create-modal.modal-footer">
					<Button onClick={ModalCommands.pop} variant="secondary" data-flx="channel.channel-create-modal.button.pop">
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
					<Button
						type="submit"
						submitting={form.formState.isSubmitting}
						data-flx="channel.channel-create-modal.button.submit"
					>
						{i18n._(CREATE_CHANNEL_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
