---
title: How attachment expiry works on Fluxer
description: How we set attachment expiry, how access can extend it, and what to do before a file is removed
category: files-and-attachments
category_title: Files & Attachments
category_icon: paperclip
order: 1
snowflake_id: 1447193503661555712
---

Fluxer automatically expires older attachments. Smaller files stay available longer; bigger ones expire sooner. If people view a message with a file when it is close to expiring, we extend it so it stays available.

## How expiry is decided

- The clock starts when you upload. Viewing or downloading later does not restart it; we only refresh the expiry if the file is close to expiring (see below).
- Files **5 MB or smaller** keep links for about **3 years** (the longest window).
- Files near **500 MB** keep links for about **14 days** (the shortest window).
- Between **5 MB and 500 MB**, larger files get shorter windows and smaller files get longer ones.
- Files **over 500 MB** are not accepted on the current plan.

## Extending availability when accessed

- We only extend when a file is close to expiring: if a message with the file is loaded and the remaining time is inside the renewal window, we push the expiry forward.
- The renewal window depends on size. Small files can renew up to about **30 days**; the largest files renew up to about **7 days**. We cap the total lifetime to the size-based budget, so a 500 MB file will not suddenly gain an extra month.
- Multiple views inside the same window do not stack; one view is enough to refresh it. You do not have to click or download the file for this to happen.
- As long as people keep viewing the message before the file expires (and within the size-appropriate renewal window), it stays available. If no one views it and it reaches expiry, the link disappears.

## What happens after expiry

We regularly sweep expired attachments and delete them from our CDN and storage. There can be a short delay after the expiry time before removal.

## Why we expire attachments

- **Storage and bandwidth fairness**: Large media is costly to keep forever; expiring it keeps things fair for everyone.
- **Safety and privacy**: Clearing out long-lived uploads reduces the chance that old sensitive files linger.
- **Predictable limits**: Clear timeframes help you download what you need to keep.

## Keeping important files

- Download attachments you need before they expire.
- For full account exports (including attachment URLs), see <% article 1445731738851475456 %>. For deletion requests, see <% article 1445730947679911936 %>.

## Frequently asked questions

**Q: What if I have Plutonium, do my files stay for longer?**
At this time, all users, whether they have Plutonium or not, are subject to the same limits.

**Q: What if my workflow relies on guaranteed, persistent access to large files I am not regularly accessing?**
Fluxer is a messaging service, not a cloud storage platform. We suggest advanced users host files themselves if this is a concern. There are tools that let you upload screenshots or files to your own cloud storage and domain, then share those links on Fluxer or elsewhere while retaining full control.

**Q: Do I need to click or download a file to keep it available?**
No. If a message with the file is viewed in chat or search while the file is near expiry, we refresh its expiry window even if you do not click or download it.

**Q: What about Saved Media?**
Fluxer has a Saved Media feature that lets you keep up to **50** media files (or **500** if you have Plutonium)—images, videos, GIFs, or audio—to access across your accounts. Saved Media is **not** subject to attachment expiry; items you save there stay until you delete them.

**Q: Can I hide the "Expires on" note under attachments?**
Yes. Go to User Settings (cogwheel bottom left) > Messages & Media > Media and toggle off "Show Attachment Expiry Indicator" if you prefer not to see it.
