import { Avatar } from "@app/features/ui/components/Avatar";
import { Tooltip } from "@app/features/ui/tooltip/Tooltip";
import { observer } from "mobx-react-lite";
import { useMessageViewContext } from "./MessageViewContext";
import Users from "@app/features/user/state/Users";
import { clsx } from "clsx";
import styles from '@app/features/theme/styles/Message.module.css';
import type React from "react";
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';

export const PersonaTag = observer(({className} : React.HTMLProps<void>) => {
	const {
		message,
	} = useMessageViewContext();
	const userAuthor = Users.getUser(message.author.id);
	const userNickname = userAuthor && NicknameUtils.getNickname(userAuthor, message.guildId, message.channelId);
	const author = message.webhookId != null ? message.author : (userAuthor ?? message.author);
	return <Tooltip maxWidth="xl" text={`Account: @${author.username}#${author.discriminator}`}>
		{/* <Avatar
			user={author}
			size={16}
			guildId={message.guildId}
			className={clsx(className, styles.messageAvatar, styles.messageAvatarEmbedded)}
			data-flx="channel.message-author-info.persona-account-tag"
		/> */}
		<div className={clsx(styles.messagePersonaTag, className)}>
			{/* TODO: custom user field */}
			{userNickname || message.author.displayName}
		</div>
	</Tooltip>;
});
