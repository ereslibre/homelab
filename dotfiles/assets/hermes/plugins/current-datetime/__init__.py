from datetime import datetime


def inject_current_datetime(is_first_turn=False, platform="", **kwargs):
    if not is_first_turn or platform == "cli":
        return None

    now = datetime.now().astimezone()
    return {
        "context": (
            "Current date and time at session start: "
            f"{now.strftime('%A, %Y-%m-%d %H:%M:%S %Z %z')}"
        )
    }


def register(ctx):
    ctx.register_hook("pre_llm_call", inject_current_datetime)
