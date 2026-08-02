// SPDX-License-Identifier: AGPL-3.0-or-later

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_AVAILABILITY_META_DESCRIPTION_DESCRIPTOR = {
        key: "platform_support.availability.meta_description",
        message: "Get {product_name} for your web browser, {windows}, {linux}, and {macos}. {ios} and {android} are in public testing.",
        comment: "Download-page meta description. Preserve {product_name} exactly; keep platform names conventional and make desktop, browser, and public mobile testing availability clear. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_AVAILABILITY_SUMMARY_DESCRIPTOR = {
        key: "platform_support.availability.summary",
        message: "Available in your web browser and on {windows}, {linux}, and {macos}, with {ios} and {android} in public testing.",
        comment: "Intro copy below the download-page heading. Keep platform names conventional and make browser, desktop, and public mobile testing availability clear. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_DESKTOP_INTERFACE_LABEL_DESCRIPTOR = {
        key: "platform_support.desktop.interface_label",
        message: "{product_name} desktop interface",
        comment: "Alt text for a desktop product screenshot. Preserve {product_name} exactly; describe the image as the desktop interface, not as a download action. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_DESKTOP_LABEL_DESCRIPTOR = {
        key: "platform_support.desktop.label",
        message: "Desktop",
        comment: "Short UI label or heading in platform availability and download support copy. Keep wording clear about desktop, web, and mobile status.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_DESKTOP_USE_DESKTOP_CLIENT_MOBILE_SOON_DESCRIPTOR = {
        key: "platform_support.desktop.use_desktop_client_mobile_soon",
        message: "Use the desktop client (mobile coming soon)",
        comment: "Body copy in platform availability and download support copy. Keep wording clear about desktop, web, and mobile status.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_DESKTOP_CANARY_WINDOWS_WARNING_DESCRIPTOR = {
        key: "platform_support.desktop.canary_windows_warning",
        message: "This Canary build may be flagged by {microsoft_defender} or other antivirus software on {windows} for now. See GitHub issue {issue_link} for more details.",
        comment: "Canary-only notice below the Windows download row. Preserve Microsoft Defender and Windows as names; keep {issue_link} exactly where the linked issue number should appear and make clear the antivirus warning is temporary. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_DONE_DESKTOP_DESCRIPTOR = {
        key: "platform_support.mobile.install_as_app.done_desktop",
        message: "Done! You can now open {product_name} as if it were a regular program.",
        comment: "Body copy for mobile/PWA install guidance on the download page. Keep instructions clear, device-appropriate, and concise; preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_DONE_MOBILE_DESCRIPTOR = {
        key: "platform_support.mobile.install_as_app.done_mobile",
        message: "Done! You can now open {product_name} from your home screen.",
        comment: "Body copy for mobile/PWA install guidance on the download page. Keep instructions clear, device-appropriate, and concise; preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_IN_CHROME_DESCRIPTOR = {
        key: "platform_support.mobile.install_as_app.guides.in_chrome",
        message: " in {chrome}",
        comment: "Compact UI label for mobile/PWA install guidance on the download page. Keep instructions clear, device-appropriate, and concise; preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_IN_CHROME_OR_ANOTHER_BROWSER_DESCRIPTOR = {
        key: "platform_support.mobile.install_as_app.guides.in_chrome_or_another_browser",
        message: " in {chrome} or another browser with PWA support",
        comment: "Body copy for mobile/PWA install guidance on the download page. Keep instructions clear, device-appropriate, and concise; preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_IN_SAFARI_DESCRIPTOR = {
        key: "platform_support.mobile.install_as_app.guides.in_safari",
        message: " in Safari",
        comment: "Compact UI label for mobile/PWA install guidance on the download page. Keep instructions clear, device-appropriate, and concise; preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_PWA_INSTALLATION_GUIDE_DESCRIPTOR = {
        key: "platform_support.mobile.install_as_app.guides.pwa_installation_guide",
        message: "PWA installation guide for {name}",
        comment: "Compact UI label for mobile/PWA install guidance on the download page. Keep instructions clear, device-appropriate, and concise; preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_STEPS_PRESS_ADD_TO_HOME_SCREEN_DESCRIPTOR = {
        key: "platform_support.mobile.install_as_app.guides.steps.press_add_to_home_screen",
        message: "Press \"Add to home screen\"",
        comment: "Compact UI label for mobile/PWA install guidance on the download page. Keep instructions clear, device-appropriate, and concise; preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_STEPS_PRESS_ADD_UPPER_RIGHT_DESCRIPTOR = {
        key: "platform_support.mobile.install_as_app.guides.steps.press_add_upper_right",
        message: "Press \"Add\" in the upper-right corner",
        comment: "Body copy for mobile/PWA install guidance on the download page. Keep instructions clear, device-appropriate, and concise; preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_STEPS_PRESS_INSTALL_APP_DESCRIPTOR = {
        key: "platform_support.mobile.install_as_app.guides.steps.press_install_app",
        message: "Press \"Install app\"",
        comment: "Compact UI label for mobile/PWA install guidance on the download page. Keep instructions clear, device-appropriate, and concise; preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_STEPS_PRESS_INSTALL_BUTTON_ADDRESS_BAR_DESCRIPTOR = {
        key: "platform_support.mobile.install_as_app.guides.steps.press_install_button_address_bar",
        message: "Press the install button (downward-pointing arrow on monitor) in the address bar",
        comment: "Button or link label for mobile/PWA install guidance on the download page. Keep instructions clear, device-appropriate, and concise; preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_STEPS_PRESS_INSTALL_IN_POPUP_DESCRIPTOR = {
        key: "platform_support.mobile.install_as_app.guides.steps.press_install_in_popup",
        message: "Press \"Install\" in the popup that appears",
        comment: "Body copy for mobile/PWA install guidance on the download page. Keep instructions clear, device-appropriate, and concise; preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_STEPS_PRESS_MORE_MENU_DESCRIPTOR = {
        key: "platform_support.mobile.install_as_app.guides.steps.press_more_menu",
        message: "Press the \"More\" (⋮) button in the top-right corner",
        comment: "Body copy for mobile/PWA install guidance on the download page. Keep instructions clear, device-appropriate, and concise; preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_STEPS_PRESS_SHARE_BUTTON_DESCRIPTOR = {
        key: "platform_support.mobile.install_as_app.guides.steps.press_share_button",
        message: "Press the share button (rectangle with upward-pointing arrow)",
        comment: "Button or link label for mobile/PWA install guidance on the download page. Keep instructions clear, device-appropriate, and concise; preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_INSTALL_FLUXER_AS_APP_DESCRIPTOR = {
        key: "platform_support.mobile.install_as_app.install_fluxer_as_app",
        message: "Install {product_name} as an app",
        comment: "Compact UI label for mobile/PWA install guidance on the download page. Keep instructions clear, device-appropriate, and concise; preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_LINK_DESCRIPTOR = {
        key: "platform_support.mobile.install_as_app.link",
        message: "Installing as an app",
        comment: "Secondary text-link label on the download page that opens the PWA installation guide. Keep it short and action-oriented.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_TITLE_DESCRIPTOR = {
        key: "platform_support.mobile.install_as_app.title",
        message: "How to install as an app",
        comment: "Short UI label or heading for mobile/PWA install guidance on the download page. Keep instructions clear, device-appropriate, and concise; preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_INTERFACE_LABEL_DESCRIPTOR = {
        key: "platform_support.mobile.interface_label",
        message: "{product_name} mobile interface",
        comment: "Short UI label or heading for mobile/PWA install guidance on the download page. Keep instructions clear, device-appropriate, and concise; preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_ANDROID_APK_DESCRIPTOR = {
        key: "platform_support.platforms.android.apk",
        message: "APK",
        comment: "Compact UI label naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_ANDROID_MIN_VERSION_DESCRIPTOR = {
        key: "platform_support.platforms.android.min_version",
        message: "{android} 8+",
        comment: "Compact UI label naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_ANDROID_NAME_DESCRIPTOR = {
        key: "platform_support.platforms.android.name",
        message: "{android}",
        comment: "Short UI label or heading naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_IOS_IOS_IPADOS_DESCRIPTOR = {
        key: "platform_support.platforms.ios.ios_ipados",
        message: "{ios} and {ipados}",
        comment: "Compact UI label naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_IOS_MIN_VERSION_DESCRIPTOR = {
        key: "platform_support.platforms.ios.min_version",
        message: "{ios} 15+",
        comment: "Compact UI label naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_IOS_NAME_DESCRIPTOR = {
        key: "platform_support.platforms.ios.name",
        message: "{ios}",
        comment: "Short UI label or heading naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_IOS_TESTFLIGHT_DESCRIPTOR = {
        key: "platform_support.platforms.ios.testflight",
        message: "{testflight}",
        comment: "Compact UI label naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_LINUX_CHOOSE_DISTRIBUTION_DESCRIPTOR = {
        key: "platform_support.platforms.linux.choose_distribution",
        message: "Choose {linux} distribution",
        comment: "Compact UI label naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_LINUX_NAME_DESCRIPTOR = {
        key: "platform_support.platforms.linux.name",
        message: "{linux}",
        comment: "Short UI label or heading naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_LINUX_RECOMMENDED_DESCRIPTOR = {
        key: "platform_support.platforms.linux.recommended",
        message: "recommended",
        comment: "Compact UI label naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_MACOS_APPLE_SILICON_DESCRIPTOR = {
        key: "platform_support.platforms.macos.apple_silicon",
        message: "{apple_silicon}",
        comment: "Compact UI label naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_MACOS_DOWNLOAD_LABEL_DESCRIPTOR = {
        key: "platform_support.platforms.macos.download_label",
        message: "Download for {macos}",
        comment: "Button or link label naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_MACOS_INTEL_DESCRIPTOR = {
        key: "platform_support.platforms.macos.intel",
        message: "Intel",
        comment: "Compact UI label naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_MACOS_MIN_VERSION_DESCRIPTOR = {
        key: "platform_support.platforms.macos.min_version",
        message: "{macos} 10.15+",
        comment: "Compact UI label naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_MACOS_NAME_DESCRIPTOR = {
        key: "platform_support.platforms.macos.name",
        message: "{macos}",
        comment: "Short UI label or heading naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_WINDOWS_DOWNLOAD_LABEL_DESCRIPTOR = {
        key: "platform_support.platforms.windows.download_label",
        message: "Download for {windows}",
        comment: "Button or link label naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_WINDOWS_MIN_VERSION_DESCRIPTOR = {
        key: "platform_support.platforms.windows.min_version",
        message: "{windows} 10+",
        comment: "Compact UI label naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_WINDOWS_NAME_DESCRIPTOR = {
        key: "platform_support.platforms.windows.name",
        message: "{windows}",
        comment: "Short UI label or heading naming a platform, installer, architecture, or minimum version in download UI. Keep platform names conventional and labels compact. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_PLATFORMS_PORTABLE_DESCRIPTOR = {
        key: "platform_support.platforms.portable",
        message: "Portable",
        comment: "Compact UI label for a portable (no-install) desktop build that stores all data next to the executable. Keep it short.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_WEB_APP_TITLE_DESCRIPTOR = {
        key: "platform_support.mobile.web_app.title",
        message: "Web app",
        comment: "Card title for the mobile web app (PWA) option on the download page. Keep it short.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_WEB_APP_BODY_DESCRIPTOR = {
        key: "platform_support.mobile.web_app.body",
        message: "{product_name} runs in any desktop or mobile web browser, and installs to your home screen or desktop like a Progressive Web App. It is the most complete way to use {product_name} on a phone today.",
        comment: "Body copy for the web app row on the download page. Preserve {product_name} exactly; mention desktop and mobile browsers and that it installs like a Progressive Web App. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_IOS_TITLE_DESCRIPTOR = {
        key: "platform_support.mobile.ios.title",
        message: "{ios} app",
        comment: "Card title for the iOS app option on the download page. Keep the platform name conventional and short. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_IOS_BODY_DESCRIPTOR = {
        key: "platform_support.mobile.ios.body",
        message: "The {ios} app on {testflight} is currently limited to {premium_tier_full_name} members. Public access is coming soon. The full {product_name} web app also works in Safari and can be added to your Home Screen.",
        comment: "Body copy for the iOS app row on the download page. Make clear that TestFlight access is currently limited to Fluxer Plutonium members, that public access is coming soon, and that the web app remains available in Safari. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_ANDROID_TITLE_DESCRIPTOR = {
        key: "platform_support.mobile.android.title",
        message: "{android} app",
        comment: "Card title for the Android app option on the download page. Keep the platform name conventional and short. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_ANDROID_BODY_DESCRIPTOR = {
        key: "platform_support.mobile.android.body",
        message: "Install the {android} APK straight from our open source repository on {github}.",
        comment: "Body copy for the Android app card on the download page. Keep APK and GitHub as proper names; make clear the install file lives in the open source repository. Preserve placeholders exactly.",
    };
);

crate::marketing_message!(
    pub const PLATFORM_SUPPORT_MOBILE_ANDROID_CTA_DESCRIPTOR = {
        key: "platform_support.mobile.android.cta",
        message: "Download the APK",
        comment: "Button or link label on the Android app card that opens the GitHub repository where the APK is published. Keep APK as a proper name; keep it short.",
    };
);
