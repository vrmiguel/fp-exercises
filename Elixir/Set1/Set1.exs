defmodule Is do
  def even?(x) do
    rem(x, 2) == 0
  end
end

defmodule PasswordBank do
  def is_in?(word) do
    case word do
      "mellon"    -> :true
      "swordfish" -> :true
      _           -> :false
    end
  end
end

defmodule MoocSet1 do

  def double(x) do
    x+x
  end

  def quadruple(x) do
    x |> double |> double
  end

  def distance(xi, yi, xf, yf) do
    xdist = (xf - xi) * (xf - xi)
    ydist = (yf - yi) * (yf - yi)
    :math.sqrt(xdist + ydist)
  end

  def eeny(x) do
    case Is.even?(x) do
      :true -> "eeny"
      _     -> "meeny"
    end
  end

  def check_password(pw) do
    if PasswordBank.is_in?(pw) do
      "You're in."
    else
      "ACCESS DENIED!"
    end
  end

  def is_zero?(0), do: :true
  def is_zero?(_), do: :false

  def sum_to(1), do: 1
  def sum_to(x), do: x + sum_to(x-1)

  def power(_, 0), do: 1
  def power(n, k), do: n * power(n, k-1)

  def ilog3(0), do: 0
  def ilog3(x), do: 1 + ilog3(div(x, 3))

end

MoocSet1.double(3)                |> IO.puts
MoocSet1.quadruple(3)             |> IO.puts
MoocSet1.distance(1, 1, 4, 5)     |> IO.puts
MoocSet1.check_password("mellon") |> IO.puts
MoocSet1.is_zero?(0)              |> IO.puts
MoocSet1.sum_to(4)                |> IO.puts
MoocSet1.power(4, 4)              |> IO.puts
MoocSet1.ilog3(7)                 |> IO.puts
