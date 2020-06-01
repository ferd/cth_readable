defmodule CthReadable do
  import Exception

  def format_exception({exc, trace}) do
    if exception?(exc) do
        message(exc) <> "\nstacktrace:\n" <> format_stacktrace(trace)
    else
      false
    end
  end

  def format_exception(_) do
    false
  end
end
