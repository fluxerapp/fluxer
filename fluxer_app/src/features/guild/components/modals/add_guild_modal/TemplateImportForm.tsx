// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {THE_OTHER_PLATFORM_TEMPLATE_EXAMPLE_URL} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import styles from '@app/features/guild/components/modals/AddGuildModal.module.css';
import {
	renderGuildCreateAccountGate,
	renderGuildCreateFields,
	useGuildCreateIconField,
} from '@app/features/guild/components/modals/add_guild_modal/GuildCreateShared';
import {
	CREATE_FROM_TEMPLATE_FORM_DESCRIPTOR,
	handleGuildCreationError,
	IMPORT_TEMPLATE_FORM_DESCRIPTOR,
	INVALID_JSON_PLEASE_PASTE_THE_FULL_RESPONSE_FROM_DESCRIPTOR,
	isTemplateEveryoneRole,
	ModalFooterContext,
	mapTemplateChannelTypeToFluxer,
	PASTE_TEMPLATE_JSON_FORM_DESCRIPTOR,
	PASTE_THE_JSON_RESPONSE_HERE_DESCRIPTOR,
	PLEASE_ENTER_A_VALID_THE_OTHER_PLATFORM_TEMPLATE_URL_OR_DESCRIPTOR,
	parseTemplateCode,
	TEMPLATE_JSON_DESCRIPTOR,
	TEMPLATE_URL_DESCRIPTOR,
	type TemplateCreateFormInputs,
	type TemplateImportFormInputs,
	type TemplateJsonFormInputs,
	THE_OTHER_PLATFORM_TEMPLATE_NOT_FOUND_DESCRIPTOR,
	THE_OTHER_PLATFORM_TEMPLATE_RATE_LIMITED_DESCRIPTOR,
	THE_OTHER_PLATFORM_TEMPLATE_UNREADABLE_DESCRIPTOR,
	THIS_DOESN_T_LOOK_LIKE_A_VALID_TEMPLATE_DESCRIPTOR,
} from '@app/features/guild/components/modals/add_guild_modal/shared';
import {CREATE_COMMUNITY_DESCRIPTOR, NEXT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input, Textarea} from '@app/features/ui/components/form/FormInput';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {THE_OTHER_PLATFORM} from '@fluxer/constants/src/ExternalPlatformConstants';
import {OtherPlatformTemplateFetchFailure} from '@fluxer/desktop_ipc/src/OtherPlatformTemplateContract';
import type {TemplateSerializedGuild} from '@fluxer/schema/src/domains/guild/GuildTemplateSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowSquareOutIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useContext, useEffect, useId, useMemo, useRef, useState} from 'react';
import {useForm} from 'react-hook-form';
import * as v from 'valibot';

const TemplateEntityId = v.union([
	v.pipe(v.number(), v.integer(), v.minValue(0)),
	v.pipe(v.string(), v.regex(/^\d+$/)),
]);
const PermissionBitfield = v.union([v.string(), v.pipe(v.number(), v.integer(), v.minValue(0))]);
const CHANGE_ICON_DESCRIPTOR = msg({
	message: 'Change icon',
	comment:
		'Title of the modal where the user picks an imported-template community icon source. Keep it concise. Keep the tone plain and specific.',
});
const OverwriteType = v.pipe(
	v.union([v.number(), v.string()]),
	v.transform((value) => {
		if (typeof value === 'string') {
			if (value === 'role') return 0;
			if (value === 'member') return 1;
			return Number(value);
		}
		return value;
	}),
);
const TemplateChannelSchema = v.looseObject({
	id: TemplateEntityId,
	type: v.number(),
	name: v.nullish(v.string(), ''),
	topic: v.nullish(v.string()),
	position: v.number(),
	parent_id: v.nullish(TemplateEntityId),
	bitrate: v.nullish(v.number()),
	user_limit: v.nullish(v.number()),
	voice_connection_limit: v.nullish(v.number()),
	nsfw: v.nullish(v.boolean()),
	rate_limit_per_user: v.nullish(v.number()),
	permission_overwrites: v.nullish(
		v.array(v.object({id: TemplateEntityId, type: OverwriteType, allow: PermissionBitfield, deny: PermissionBitfield})),
	),
});
const TemplateRoleSchema = v.object({
	id: TemplateEntityId,
	name: v.nullish(v.string(), ''),
	permissions: v.optional(PermissionBitfield),
	permissions_new: v.optional(PermissionBitfield),
	color: v.optional(v.number()),
	hoist: v.optional(v.boolean()),
	mentionable: v.optional(v.boolean()),
	unicode_emoji: v.nullish(v.string()),
});
const GuildTemplateResponseSchema = v.object({
	code: v.string(),
	name: v.string(),
	description: v.nullish(v.string()),
	serialized_source_guild: v.object({
		name: v.string(),
		description: v.nullish(v.string()),
		verification_level: v.optional(v.number()),
		default_message_notifications: v.optional(v.number()),
		explicit_content_filter: v.optional(v.number()),
		system_channel_id: v.nullish(TemplateEntityId),
		afk_timeout: v.optional(v.number()),
		system_channel_flags: v.optional(v.number()),
		roles: v.array(TemplateRoleSchema),
		channels: v.array(TemplateChannelSchema),
	}),
});

type TemplateImportStep = 'url' | 'json' | 'create';
type TemplateResponseOutcome = 'applied' | 'invalid';

const logger = new Logger('TemplateImportForm');

const TEMPLATE_IMPORT_STEP_ORDER: ReadonlyArray<TemplateImportStep> = ['url', 'json', 'create'];
const TEMPLATE_STATS_DESCRIPTOR = msg({
	message:
		'{textChannelCount, plural, one {# text channel} other {# text channels}}, {voiceChannelCount, plural, one {# voice channel} other {# voice channels}}, {categoryCount, plural, one {# category} other {# categories}}, {roleCount, plural, one {# role} other {# roles}}',
	comment:
		'Template preview summary listing how many text channels, voice channels, categories, and roles will be imported.',
});
const TEMPLATE_STATS_WITH_FORUMS_DESCRIPTOR = msg({
	message:
		'{textChannelCount, plural, one {# text channel} other {# text channels}}, {forumChannelCount, plural, one {# forum channel} other {# forum channels}}, {voiceChannelCount, plural, one {# voice channel} other {# voice channels}}, {categoryCount, plural, one {# category} other {# categories}}, {roleCount, plural, one {# role} other {# roles}}',
	comment:
		'Template preview summary listing how many text channels, forum channels, voice channels, categories, and roles will be imported. Used when the template has at least one forum or media channel.',
});
const COULDN_T_LOAD_THE_TEMPLATE_AUTOMATICALLY_DESCRIPTOR = msg({
	message:
		"Couldn't load the template automatically. Open the link below in your browser, then copy the entire JSON response and paste it into the box.",
	comment:
		'Instructions in the desktop app import-template flow shown when looking up the template directly failed, so the user has to fetch the JSON in a browser and paste it instead. "JSON" must not be translated.',
});
const IMAGE_COULDN_T_BE_USED_DESCRIPTOR = msg({
	message: "Image couldn't be used",
	comment: 'Error modal title shown when a template-import community icon upload cannot be accepted or processed.',
});
const ICON_FIELD_MESSAGES = {
	imageCouldNotBeUsed: IMAGE_COULDN_T_BE_USED_DESCRIPTOR,
	changeIcon: CHANGE_ICON_DESCRIPTOR,
};
export const TemplateImportForm = observer(() => {
	const {i18n} = useLingui();
	const theOtherPlatform = THE_OTHER_PLATFORM;
	const [step, setStep] = useState<TemplateImportStep>('url');
	const [apiUrl, setApiUrl] = useState<string | null>(null);
	const [templateData, setTemplateData] = useState<TemplateSerializedGuild | null>(null);
	const [templateName, setTemplateName] = useState<string | null>(null);
	const [loadedDirectly, setLoadedDirectly] = useState(false);
	const [directLoadFailed, setDirectLoadFailed] = useState(false);
	const lookupInFlightRef = useRef(false);
	const urlForm = useForm<TemplateImportFormInputs>({defaultValues: {url: ''}});
	const jsonForm = useForm<TemplateJsonFormInputs>({defaultValues: {json: ''}});
	const createForm = useForm<TemplateCreateFormInputs>({defaultValues: {name: ''}});
	const modalFooterContext = useContext(ModalFooterContext);
	const urlFormId = useId();
	const jsonFormId = useId();
	const createFormId = useId();
	const applyTemplateResponse = useCallback(
		(json: unknown): TemplateResponseOutcome => {
			const parsed = v.safeParse(GuildTemplateResponseSchema, json);
			if (!parsed.success) return 'invalid';
			setTemplateData(parsed.output.serialized_source_guild);
			setTemplateName(parsed.output.name);
			createForm.setValue('name', parsed.output.serialized_source_guild.name);
			return 'applied';
		},
		[createForm],
	);
	const fallBackToPaste = useCallback((code: string, directLoadAttempted: boolean) => {
		setApiUrl(`https://discord.com/api/guilds/templates/${code}`);
		setLoadedDirectly(false);
		setDirectLoadFailed(directLoadAttempted);
		setStep('json');
	}, []);
	const handleResolveUrl = useCallback(
		async (data: TemplateImportFormInputs) => {
			const code = parseTemplateCode(data.url);
			const invalidCodeMessage = i18n._(PLEASE_ENTER_A_VALID_THE_OTHER_PLATFORM_TEMPLATE_URL_OR_DESCRIPTOR, {
				theOtherPlatform: THE_OTHER_PLATFORM,
			});
			if (!code) {
				urlForm.setError('url', {message: invalidCodeMessage});
				return;
			}
			const templateApi = getElectronAPI()?.otherPlatformTemplate;
			if (typeof templateApi?.fetch !== 'function') {
				fallBackToPaste(code, false);
				return;
			}
			if (lookupInFlightRef.current) return;
			lookupInFlightRef.current = true;
			let result: Awaited<ReturnType<typeof templateApi.fetch>>;
			try {
				result = await templateApi.fetch(code);
			} catch (error) {
				logger.warn('Direct template lookup failed, falling back to paste', error);
				fallBackToPaste(code, true);
				return;
			} finally {
				lookupInFlightRef.current = false;
			}
			if (result.ok) {
				if (applyTemplateResponse(result.template) === 'invalid') {
					urlForm.setError('url', {
						message: i18n._(THE_OTHER_PLATFORM_TEMPLATE_UNREADABLE_DESCRIPTOR, {theOtherPlatform: THE_OTHER_PLATFORM}),
					});
					return;
				}
				setLoadedDirectly(true);
				setStep('create');
				return;
			}
			switch (result.failure) {
				case OtherPlatformTemplateFetchFailure.INVALID_CODE:
					urlForm.setError('url', {message: invalidCodeMessage});
					return;
				case OtherPlatformTemplateFetchFailure.NOT_FOUND:
					urlForm.setError('url', {
						message: i18n._(THE_OTHER_PLATFORM_TEMPLATE_NOT_FOUND_DESCRIPTOR, {theOtherPlatform: THE_OTHER_PLATFORM}),
					});
					return;
				case OtherPlatformTemplateFetchFailure.RATE_LIMITED:
					urlForm.setError('url', {
						message: i18n._(THE_OTHER_PLATFORM_TEMPLATE_RATE_LIMITED_DESCRIPTOR, {
							theOtherPlatform: THE_OTHER_PLATFORM,
						}),
					});
					return;
				case OtherPlatformTemplateFetchFailure.NETWORK:
				case OtherPlatformTemplateFetchFailure.INVALID_RESPONSE:
					fallBackToPaste(code, true);
					return;
			}
		},
		[urlForm, i18n, applyTemplateResponse, fallBackToPaste],
	);
	const {handleSubmit: handleUrlSubmit, isSubmitting: isResolvingUrl} = useFormSubmit({
		form: urlForm,
		onSubmit: handleResolveUrl,
		defaultErrorField: 'url',
	});
	const handleParseJson = useCallback(
		(data: TemplateJsonFormInputs) => {
			let json: unknown;
			try {
				json = JSON.parse(data.json);
			} catch {
				jsonForm.setError('json', {message: i18n._(INVALID_JSON_PLEASE_PASTE_THE_FULL_RESPONSE_FROM_DESCRIPTOR)});
				return;
			}
			if (applyTemplateResponse(json) === 'invalid') {
				jsonForm.setError('json', {message: i18n._(THIS_DOESN_T_LOOK_LIKE_A_VALID_TEMPLATE_DESCRIPTOR)});
				return;
			}
			setStep('create');
		},
		[jsonForm, i18n, applyTemplateResponse],
	);
	const {handleSubmit: handleJsonSubmit} = useFormSubmit({
		form: jsonForm,
		onSubmit: handleParseJson,
		defaultErrorField: 'json',
	});
	const handleCreateFromTemplate = useCallback(
		async (data: TemplateCreateFormInputs) => {
			if (!templateData) return;
			try {
				const guild = await GuildCommands.createFromTemplate({
					name: data.name,
					icon: data.icon,
					template: templateData,
				});
				ModalCommands.pop();
				NavigationCommands.selectChannel(guild.id, guild.system_channel_id || undefined);
			} catch (error) {
				handleGuildCreationError(error);
			}
		},
		[templateData],
	);
	const {handleSubmit: handleCreateSubmit, isSubmitting: isCreating} = useFormSubmit({
		form: createForm,
		onSubmit: handleCreateFromTemplate,
		defaultErrorField: 'name',
	});
	const nameValue = createForm.watch('name');
	const urlValue = urlForm.watch('url');
	const jsonValue = jsonForm.watch('json');
	const iconField = useGuildCreateIconField({
		form: createForm,
		nameValue,
		dataFlx: 'guild.add-guild-modal.template-import-form',
		cropModalDataFlx: 'guild.add-guild-modal.handle-icon-upload.asset-crop-modal--2',
		clearIconErrors: false,
		messages: ICON_FIELD_MESSAGES,
	});
	const templateStats = useMemo(() => {
		if (!templateData) return null;
		const channelTypes = templateData.channels
			.map((channel) => mapTemplateChannelTypeToFluxer(channel.type))
			.filter((channelType): channelType is number => channelType !== null);
		const countOf = (...types: ReadonlyArray<number>) =>
			channelTypes.filter((channelType) => types.includes(channelType)).length;
		return {
			textChannels: countOf(ChannelTypes.GUILD_TEXT, ChannelTypes.GUILD_ANNOUNCEMENT),
			forumChannels: countOf(ChannelTypes.GUILD_FORUM, ChannelTypes.GUILD_MEDIA),
			voiceChannels: countOf(ChannelTypes.GUILD_VOICE),
			categories: countOf(ChannelTypes.GUILD_CATEGORY),
			roles: templateData.roles.filter((role) => !isTemplateEveryoneRole(role)).length,
		};
	}, [templateData]);
	const handleOpenApiUrl = useCallback(() => {
		if (apiUrl) {
			openExternalUrlWithWarning(apiUrl);
		}
	}, [apiUrl]);
	useEffect(() => {
		if (step === 'create') {
			const isNameEmpty = !nameValue?.trim();
			modalFooterContext?.setFooterContent(
				<>
					<Button
						onClick={() => setStep(loadedDirectly ? 'url' : 'json')}
						variant="secondary"
						data-flx="guild.add-guild-modal.template-import-form.button.back-to-json"
					>
						<Trans>Back</Trans>
					</Button>
					<Button
						onClick={handleCreateSubmit}
						submitting={isCreating}
						disabled={isNameEmpty}
						data-flx="guild.add-guild-modal.template-import-form.button.create-submit"
					>
						{i18n._(CREATE_COMMUNITY_DESCRIPTOR)}
					</Button>
				</>,
			);
		} else if (step === 'json') {
			const isJsonEmpty = !jsonValue?.trim();
			modalFooterContext?.setFooterContent(
				<>
					<Button
						onClick={() => setStep('url')}
						variant="secondary"
						data-flx="guild.add-guild-modal.template-import-form.button.set-step"
					>
						<Trans>Back</Trans>
					</Button>
					<Button
						onClick={handleJsonSubmit}
						disabled={isJsonEmpty}
						data-flx="guild.add-guild-modal.template-import-form.button.json-submit"
					>
						{i18n._(NEXT_DESCRIPTOR)}
					</Button>
				</>,
			);
		} else {
			const isUrlEmpty = !urlValue?.trim();
			modalFooterContext?.setFooterContent(
				<>
					<Button
						onClick={modalFooterContext?.onBack}
						variant="secondary"
						data-flx="guild.add-guild-modal.template-import-form.button.back-to-landing"
					>
						<Trans>Back</Trans>
					</Button>
					<Button
						onClick={handleUrlSubmit}
						submitting={isResolvingUrl}
						disabled={isUrlEmpty}
						data-flx="guild.add-guild-modal.template-import-form.button.url-submit"
					>
						{i18n._(NEXT_DESCRIPTOR)}
					</Button>
				</>,
			);
		}
		return () => modalFooterContext?.setFooterContent(null);
	}, [
		step,
		handleUrlSubmit,
		handleJsonSubmit,
		handleCreateSubmit,
		isCreating,
		isResolvingUrl,
		loadedDirectly,
		modalFooterContext,
		nameValue,
		urlValue,
		jsonValue,
	]);
	const accountGate = renderGuildCreateAccountGate(i18n, 'guild.add-guild-modal.template-import-form');
	if (accountGate) {
		return accountGate;
	}
	const renderUrlStep = (): React.ReactNode => (
		<div className={styles.formContainer} data-flx="guild.add-guild-modal.template-import-form.form-container--3">
			<Modal.Description data-flx="guild.add-guild-modal.template-import-form.modal-description">
				<Trans>Paste a {theOtherPlatform} template URL to import its structure into a new community.</Trans>
			</Modal.Description>
			<Form
				form={urlForm}
				onSubmit={handleUrlSubmit}
				id={urlFormId}
				aria-label={i18n._(IMPORT_TEMPLATE_FORM_DESCRIPTOR)}
				data-flx="guild.add-guild-modal.template-import-form.form.url-submit"
			>
				<div className={styles.iconSection} data-flx="guild.add-guild-modal.template-import-form.icon-section">
					<Input
						data-flx="guild.add-guild-modal.template-import-form.input.text"
						{...urlForm.register('url')}
						autoFocus={true}
						error={urlForm.formState.errors.url?.message}
						label={i18n._(TEMPLATE_URL_DESCRIPTOR)}
						name="url"
						placeholder={THE_OTHER_PLATFORM_TEMPLATE_EXAMPLE_URL}
						readOnly={isResolvingUrl}
						required={true}
						type="text"
					/>
				</div>
			</Form>
		</div>
	);
	const renderJsonStep = (): React.ReactNode => (
		<div className={styles.formContainer} data-flx="guild.add-guild-modal.template-import-form.form-container--4">
			<Modal.Description data-flx="guild.add-guild-modal.template-import-form.modal-description--2">
				{directLoadFailed ? (
					i18n._(COULDN_T_LOAD_THE_TEMPLATE_AUTOMATICALLY_DESCRIPTOR)
				) : (
					<Trans>
						Open the link below in your browser, then copy the entire JSON response and paste it into the box.
					</Trans>
				)}
			</Modal.Description>
			<Button
				variant="secondary"
				onClick={handleOpenApiUrl}
				data-flx="guild.add-guild-modal.template-import-form.button.open-api-url"
			>
				<span
					className={styles.openApiUrlButton}
					data-flx="guild.add-guild-modal.template-import-form.open-api-url-button"
				>
					<ArrowSquareOutIcon
						size={remFromPx(16)}
						data-flx="guild.add-guild-modal.template-import-form.arrow-square-out-icon"
					/>
					<Trans>Open template JSON</Trans>
				</span>
			</Button>
			<Form
				form={jsonForm}
				onSubmit={handleJsonSubmit}
				id={jsonFormId}
				aria-label={i18n._(PASTE_TEMPLATE_JSON_FORM_DESCRIPTOR)}
				data-flx="guild.add-guild-modal.template-import-form.form.json-submit"
			>
				<div className={styles.iconSection} data-flx="guild.add-guild-modal.template-import-form.icon-section--2">
					<Textarea
						data-flx="guild.add-guild-modal.template-import-form.textarea"
						{...jsonForm.register('json')}
						autoFocus={true}
						error={jsonForm.formState.errors.json?.message}
						label={i18n._(TEMPLATE_JSON_DESCRIPTOR)}
						name="json"
						placeholder={i18n._(PASTE_THE_JSON_RESPONSE_HERE_DESCRIPTOR)}
						required={true}
						rows={6}
					/>
				</div>
			</Form>
		</div>
	);
	const renderCreateStep = (): React.ReactNode => (
		<div className={styles.formContainer} data-flx="guild.add-guild-modal.template-import-form.form-container--5">
			{templateName && (
				<div className={styles.templatePreview} data-flx="guild.add-guild-modal.template-import-form.template-preview">
					<div className={styles.templateInfo} data-flx="guild.add-guild-modal.template-import-form.template-info">
						<span className={styles.templateLabel} data-flx="guild.add-guild-modal.template-import-form.template-label">
							<Trans>Template</Trans>
						</span>
						<span className={styles.templateName} data-flx="guild.add-guild-modal.template-import-form.template-name">
							{templateName}
						</span>
					</div>
					{templateStats && (
						<div className={styles.templateStats} data-flx="guild.add-guild-modal.template-import-form.template-stats">
							<span data-flx="guild.add-guild-modal.template-import-form.span">
								{templateStats.forumChannels > 0
									? i18n._(TEMPLATE_STATS_WITH_FORUMS_DESCRIPTOR, {
											textChannelCount: templateStats.textChannels,
											forumChannelCount: templateStats.forumChannels,
											voiceChannelCount: templateStats.voiceChannels,
											categoryCount: templateStats.categories,
											roleCount: templateStats.roles,
										})
									: i18n._(TEMPLATE_STATS_DESCRIPTOR, {
											textChannelCount: templateStats.textChannels,
											voiceChannelCount: templateStats.voiceChannels,
											categoryCount: templateStats.categories,
											roleCount: templateStats.roles,
										})}
							</span>
						</div>
					)}
				</div>
			)}
			<Form
				form={createForm}
				onSubmit={handleCreateSubmit}
				id={createFormId}
				aria-label={i18n._(CREATE_FROM_TEMPLATE_FORM_DESCRIPTOR)}
				data-flx="guild.add-guild-modal.template-import-form.form.create-submit"
			>
				{renderGuildCreateFields({
					...iconField,
					i18n,
					form: createForm,
					dataFlx: 'guild.add-guild-modal.template-import-form',
					iconSectionDataFlx: 'guild.add-guild-modal.template-import-form.icon-section--3',
					nameInputDataFlx: 'guild.add-guild-modal.template-import-form.input.text--2',
					showIconError: false,
				})}
			</Form>
		</div>
	);
	const renderStepContent = (): React.ReactNode => {
		switch (step) {
			case 'url':
				return renderUrlStep();
			case 'json':
				return renderJsonStep();
			case 'create':
				return renderCreateStep();
		}
	};
	return (
		<SteppedCarousel
			step={step}
			steps={TEMPLATE_IMPORT_STEP_ORDER}
			data-flx="guild.add-guild-modal.template-import-form.stepped-carousel"
		>
			{renderStepContent()}
		</SteppedCarousel>
	);
});
