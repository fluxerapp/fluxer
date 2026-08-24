// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SettingsTabContainer} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import {MessageAttachments} from '@app/features/channel/components/MessageAttachments';
import {MessageViewContextProvider} from '@app/features/channel/components/MessageViewContext';
import {Channel} from '@app/features/channel/models/Channel';
import {CodeBlockRenderer} from '@app/features/messaging/components/markdown/renderers/common/CodeElements';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {Message} from '@app/features/messaging/models/MessagingMessage';
import {NodeType} from '@app/features/messaging/utils/markdown/parser/Enums';
import {http} from '@app/features/platform/transport/RestTransport';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import styles from '@app/features/user/components/modals/tabs/EmbedDebuggerTab.module.css';
import * as FormUtils from '@app/lib/forms';
import {ChannelTypes, MessagePreviewContext, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {
	EmbedAuthorResponse,
	EmbedFooterResponse,
	EmbedMediaResponse,
	MessageEmbed,
	MessageEmbedResponse,
} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo, useState} from 'react';

const URL_DESCRIPTOR = msg({
	message: 'URL',
	comment: 'Short label for the URL input in the embed debugger.',
});
const UNFURL_DESCRIPTOR = msg({
	message: 'Unfurl',
	comment: 'Button label in the embed debugger.',
});
const EMBED_DEBUGGER_FORM_DESCRIPTOR = msg({
	message: 'Embed debugger form',
	comment: 'Accessible label for the embed debugger URL form.',
});
const UNABLE_TO_UNFURL_URL_DESCRIPTOR = msg({
	message: 'Unable to unfurl URL',
	comment: 'Error message shown when the embed debugger request fails.',
});

const EMBED_DEBUGGER_PREVIEW_CHANNEL_ID = '0';
let embedDebuggerPreviewChannel: Channel | null = null;

function getEmbedDebuggerPreviewChannel(): Channel {
	embedDebuggerPreviewChannel ??= new Channel({
		id: EMBED_DEBUGGER_PREVIEW_CHANNEL_ID,
		type: ChannelTypes.DM_PERSONAL_NOTES,
		name: undefined,
		topic: null,
		url: null,
		icon: null,
		owner_id: null,
		last_message_id: null,
		last_pin_timestamp: null,
		recipients: undefined,
		parent_id: null,
		bitrate: null,
		user_limit: null,
		voice_connection_limit: null,
		rtc_region: null,
		nsfw: false,
		nsfw_override: null,
		content_warning_level: 0,
		content_warning_text: null,
		rate_limit_per_user: 0,
		nicks: {},
	});
	return embedDebuggerPreviewChannel;
}

function withPreviewAuthorFallback<T extends EmbedAuthorResponse | null | undefined>(author: T): T {
	if (!author || author.proxy_icon_url || !author.icon_url) return author;
	return {...author, proxy_icon_url: author.icon_url};
}

function withPreviewFooterFallback<T extends EmbedFooterResponse | null | undefined>(footer: T): T {
	if (!footer || footer.proxy_icon_url || !footer.icon_url) return footer;
	return {...footer, proxy_icon_url: footer.icon_url};
}

function withPreviewMediaFallback<T extends EmbedMediaResponse | null | undefined>(media: T): T {
	if (!media || media.proxy_url || !media.url) return media;
	return {...media, proxy_url: media.url};
}

function normalizeEmbedForPreview(embed: MessageEmbedResponse): MessageEmbedResponse {
	return {
		...embed,
		author: withPreviewAuthorFallback(embed.author),
		footer: withPreviewFooterFallback(embed.footer),
		provider: withPreviewAuthorFallback(embed.provider),
		image: withPreviewMediaFallback(embed.image),
		thumbnail: withPreviewMediaFallback(embed.thumbnail),
		video: withPreviewMediaFallback(embed.video),
		audio: withPreviewMediaFallback(embed.audio),
		children: embed.children?.map(normalizeEmbedForPreview) ?? embed.children,
	};
}

function normalizeEmbedsForPreview(embeds: ReadonlyArray<MessageEmbedResponse>): Array<MessageEmbedResponse> {
	return embeds.map(normalizeEmbedForPreview);
}

function createPreviewMessage(url: string, embeds: ReadonlyArray<MessageEmbedResponse>): Message {
	const now = new Date().toISOString();
	return new Message(
		{
			id: '0',
			channel_id: EMBED_DEBUGGER_PREVIEW_CHANNEL_ID,
			author: {
				id: '0',
				username: 'Embed Debugger',
				discriminator: '0000',
				global_name: 'Embed Debugger',
				avatar: null,
				avatar_color: null,
				flags: 0,
				bot: false,
				system: false,
			},
			type: MessageTypes.DEFAULT,
			flags: 0,
			content: url,
			timestamp: now,
			edited_timestamp: undefined,
			pinned: false,
			mention_everyone: false,
			tts: false,
			mentions: [],
			mention_roles: [],
			mention_channels: [],
			embeds: embeds as Array<MessageEmbed>,
			attachments: [],
			stickers: [],
			nsfw_emojis: [],
			reactions: [],
		},
		{skipUserCache: true},
	);
}

const EmbedJsonCodeBlock: React.FC<{json: string}> = ({json}) => {
	const {i18n} = useLingui();
	const renderOptions = useMemo(
		() => ({
			context: MarkdownContext.STANDARD_WITHOUT_JUMBO,
			disableInteractions: true,
			shouldJumboEmojis: false,
			i18n,
		}),
		[i18n.locale],
	);
	return (
		<div className={clsx(markupStyles.markup, styles.codeSurface)} data-flx="user.embed-debugger-tab.code-surface">
			<CodeBlockRenderer
				id="embed-debugger-json"
				node={{type: NodeType.CodeBlock, language: 'json', content: json}}
				options={renderOptions}
				renderChildren={() => null}
				data-flx="user.embed-debugger-tab.embed-json-code-block.code-block-renderer"
			/>
		</div>
	);
};

const EmbedDebuggerPreview: React.FC<{message: Message}> = observer(({message}) => {
	const contextValue = useMemo(
		() => ({
			channel: getEmbedDebuggerPreviewChannel(),
			message,
			shouldGroup: false,
			isHovering: false,
			messageDisplayCompact: false,
			previewContext: MessagePreviewContext.SETTINGS,
			suppressMessageActions: true,
			handleDelete: () => {},
		}),
		[message],
	);
	return (
		<MessageViewContextProvider value={contextValue} data-flx="user.embed-debugger-tab.message-view-context-provider">
			<MessageAttachments data-flx="user.embed-debugger-tab.message-attachments" />
		</MessageViewContextProvider>
	);
});

const EmbedDebuggerTab: React.FC = observer(() => {
	const {i18n} = useLingui();
	const [url, setUrl] = useState('');
	const [submittedUrl, setSubmittedUrl] = useState('');
	const [embeds, setEmbeds] = useState<Array<MessageEmbedResponse> | null>(null);
	const [error, setError] = useState<string | null>(null);
	const [isSubmitting, setIsSubmitting] = useState(false);
	const trimmedUrl = url.trim();
	const rawJson = useMemo(() => (embeds ? JSON.stringify(embeds, null, 2) : ''), [embeds]);
	const previewEmbeds = useMemo(() => (embeds ? normalizeEmbedsForPreview(embeds) : null), [embeds]);
	const previewMessage = useMemo(
		() => (previewEmbeds ? createPreviewMessage(submittedUrl, previewEmbeds) : null),
		[previewEmbeds, submittedUrl],
	);
	const handleSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
		event.preventDefault();
		if (!trimmedUrl || isSubmitting) return;
		setIsSubmitting(true);
		setError(null);
		setEmbeds(null);
		setSubmittedUrl(trimmedUrl);
		try {
			const response = await http.post<Array<MessageEmbedResponse>>(Endpoints.UNFURL, {body: {url: trimmedUrl}});
			setEmbeds(response.body);
		} catch (err: unknown) {
			setError(FormUtils.extractErrorMessage(i18n, err) || i18n._(UNABLE_TO_UNFURL_URL_DESCRIPTOR));
		} finally {
			setIsSubmitting(false);
		}
	};
	return (
		<SettingsTabContainer data-flx="user.embed-debugger-tab.settings-tab-container">
			<SettingsSection
				id="unfurl-debugger"
				title={<Trans>Unfurl debugger</Trans>}
				description={<Trans>Fetch a URL and inspect the embed payload the chat renderer receives.</Trans>}
				data-flx="user.embed-debugger-tab.settings-tab-section"
			>
				<form
					className={styles.form}
					aria-label={i18n._(EMBED_DEBUGGER_FORM_DESCRIPTOR)}
					onSubmit={handleSubmit}
					data-flx="user.embed-debugger-tab.form.submit"
				>
					<div className={styles.formControls} data-flx="user.embed-debugger-tab.input-button-group">
						<div className={styles.urlField} data-flx="user.embed-debugger-tab.url-field">
							<Input
								className={styles.urlInput}
								label={i18n._(URL_DESCRIPTOR)}
								type="url"
								value={url}
								onChange={(event) => setUrl(event.target.value)}
								disabled={isSubmitting}
								autoComplete="off"
								spellCheck={false}
								data-flx="user.embed-debugger-tab.input.url"
							/>
						</div>
						<Button
							className={styles.submitButton}
							type="submit"
							submitting={isSubmitting}
							disabled={!trimmedUrl || isSubmitting}
							data-flx="user.embed-debugger-tab.button.submit"
						>
							{i18n._(UNFURL_DESCRIPTOR)}
						</Button>
					</div>
				</form>
				{error && (
					<div className={styles.errorText} role="alert" data-flx="user.embed-debugger-tab.error-text">
						{error}
					</div>
				)}
			</SettingsSection>
			{embeds && (
				<div className={styles.results} data-flx="user.embed-debugger-tab.results">
					<SettingsSection
						id="preview"
						linkable={false}
						title={<Trans>Preview</Trans>}
						data-flx="user.embed-debugger-tab.settings-tab-section--3"
					>
						<div className={styles.embedPreview} data-flx="user.embed-debugger-tab.embed-preview">
							{previewMessage && previewMessage.embeds.length > 0 ? (
								<EmbedDebuggerPreview message={previewMessage} data-flx="user.embed-debugger-tab.preview" />
							) : (
								<div className={styles.statusText} data-flx="user.embed-debugger-tab.status-text">
									<Trans>No embeds returned</Trans>
								</div>
							)}
						</div>
					</SettingsSection>
					<SettingsSection
						id="raw-json"
						linkable={false}
						title={<Trans>Raw embed JSON</Trans>}
						data-flx="user.embed-debugger-tab.settings-tab-section--2"
					>
						<EmbedJsonCodeBlock json={rawJson} data-flx="user.embed-debugger-tab.embed-json-code-block" />
					</SettingsSection>
				</div>
			)}
		</SettingsTabContainer>
	);
});

export default EmbedDebuggerTab;
