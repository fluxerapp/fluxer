//// Copyright (C) 2026 Fluxer Contributors
////
//// This file is part of Fluxer.
////
//// Fluxer is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as published by
//// the Free Software Foundation, either version 3 of the License, or
//// (at your option) any later version.
////
//// Fluxer is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//// GNU Affero General Public License for more details.
////
//// You should have received a copy of the GNU Affero General Public License
//// along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

import fluxer_admin/api/messages
import fluxer_admin/components/icons
import fluxer_admin/web.{type Context, href}
import gleam/list
import gleam/string
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub fn render(
  ctx: Context,
  messages: List(messages.Message),
  include_delete_button: Bool,
) {
  h.div([a.class("space-y-1")], {
    list.map(messages, fn(message) {
      render_message_row(ctx, message, include_delete_button)
    })
  })
}

fn render_message_row(
  ctx: Context,
  message: messages.Message,
  include_delete_button: Bool,
) {
  h.div(
    [
      a.class(
        "group flex items-start gap-3 px-4 py-2 hover:bg-neutral-50 transition-colors",
      ),
      a.attribute("data-message-id", message.id),
    ],
    [
      h.div([a.class("flex-shrink-0 pt-0.5")], [
        h.a(
          [
            href(ctx, "/users/" <> message.author_id),
            a.class("text-xs text-neutral-900 hover:underline cursor-pointer"),
            a.title(message.author_id),
          ],
          [element.text(message.author_username)],
        ),
        h.div([a.class("text-xs text-neutral-500")], [
          element.text(message.timestamp),
        ]),
      ]),
      h.div([a.class("flex-1 min-w-0 message-content")], [
        h.div(
          [a.class("text-sm text-neutral-900 whitespace-pre-wrap break-words")],
          [element.text(message.content)],
        ),
        case list.is_empty(message.attachments) {
          True -> element.none()
          False ->
            h.div([a.class("mt-2 space-y-1")], {
              list.map(message.attachments, fn(att) {
                h.div([a.class("text-xs flex items-center gap-1")], [
                  icons.paperclip_icon("text-neutral-500"),
                  h.a(
                    [
                      a.href(att.url),
                      a.target("_blank"),
                      a.class("text-blue-600 hover:underline"),
                    ],
                    [element.text(att.filename)],
                  ),
                ])
              })
            })
        },
        h.div([a.class("text-xs text-neutral-400 mt-1")], [
          element.text("ID: " <> message.id),
        ]),
      ]),
      case include_delete_button && !string.is_empty(message.channel_id) {
        True ->
          h.div(
            [
              a.class(
                "flex-shrink-0 opacity-0 group-hover:opacity-100 transition-opacity",
              ),
            ],
            [
              h.button(
                [
                  a.type_("button"),
                  a.class(
                    "px-2 py-1 text-xs text-red-600 hover:text-red-700 hover:bg-red-50 rounded transition-colors",
                  ),
                  a.title("Delete message"),
                  a.attribute(
                    "onclick",
                    "deleteMessage('"
                      <> message.channel_id
                      <> "', '"
                      <> message.id
                      <> "', this)",
                  ),
                ],
                [element.text("Delete")],
              ),
            ],
          )
        False -> element.none()
      },
    ],
  )
}

pub fn deletion_script() {
  "<script>
function deleteMessage(channelId, messageId, button) {
  if (!confirm('Are you sure you want to delete this message?')) {
    return;
  }

  const formData = new FormData();
  formData.append('channel_id', channelId);
  formData.append('message_id', messageId);

  button.disabled = true;
  button.textContent = 'Deleting...';

  const basePath = document.documentElement.dataset.basePath || '';
  fetch(basePath + '/messages?action=delete', {
    method: 'POST',
    body: formData
  })
  .then(response => {
    if (response.ok) {
      const messageRow = button.closest('[data-message-id]');
      if (messageRow) {
        messageRow.style.opacity = '0.5';
        messageRow.style.pointerEvents = 'none';
        const messageContent = messageRow.querySelector('.message-content');
        if (messageContent) {
          messageContent.style.textDecoration = 'line-through';
        }
      }
      const buttonContainer = button.parentElement;
      const deletedBadge = document.createElement('span');
      deletedBadge.className = 'px-2 py-1 bg-red-100 text-red-800 text-xs rounded opacity-100';
      deletedBadge.textContent = 'DELETED';
      button.replaceWith(deletedBadge);
      if (buttonContainer) {
        buttonContainer.style.opacity = '1';
      }
    } else {
      button.disabled = false;
      button.textContent = 'Delete';
      alert('Failed to delete message');
    }
  })
  .catch(error => {
    console.error('Error:', error);
    button.disabled = false;
    button.textContent = 'Delete';
    alert('Error deleting message');
  });
}
</script>"
}
